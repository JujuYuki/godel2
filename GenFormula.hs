module GenFormula where

import Mcrl
import GoParser
import Data.List as L
import Data.Maybe
import qualified Data.Map as M

  

-- chans should be the list of Sync_ and Closing_ actions
genTaus :: [String] -> String
genTaus [] = "tau + tausync"
genTaus chans@(x:xs) = (intercalate " + " chans)++" + tau + tausync"


-- OP_a in A <a>phi
allTauDiamond :: String -> [String] -> String -> String
allTauDiamond op [] fora = "<tau>"++fora++" "++op++" " ++"<tausync>"++fora
allTauDiamond op taus fora = intercalate (" "++op++" ") $ map (\x -> "<"++x++">"++fora) ("tau":"tausync":taus)

-- OP_a in A [a]phi
allTauBox :: String -> [String] -> String -> String
allTauBox op [] fora = "[tau]"++fora++" "++op++" " ++"[tausync]"++fora
allTauBox op taus fora = intercalate (" "++op++" ") $ map (\x -> "["++x++"]"++fora) ("tau":"tausync":taus)


eventuallyHolds :: [String] -> String -> String
eventuallyHolds taus fora = 
  "mu Z .("++(fora)++") || ("++(allTauDiamond "||" taus "Z")++")"

noTerminalState :: [String] -> String
noTerminalState chans = "[("++(genTaus chans)++")*]<"++(genTaus chans)++">true"

noCycle :: [String] -> String
noCycle chans = "[("++(genTaus chans)++")*](mu Y . ["++(genTaus chans)++"]Y)"

globalDeadlock :: [String] -> [(String, BarbContent)] -> String
globalDeadlock [] _ = "true % no channel"
globalDeadlock chans@(x:xs) barbs =
  if genAllBarbs == ""
  then "true % no barbs"
  else "[("++(genTaus chans)++")*]\n( <"++(genAllBarbs)++">true\n=>\n<"++(genTaus chans)++">true)"
  where
    genAllBarbs = intercalate " + " $ map printBarbs $
                  filter (isSndRcv . snd) barbs
    isSndRcv (Closed s) = False
    isSndRcv (Closing s) = False
    isSndRcv (NonEmpty s) = False
    isSndRcv (Read s) = False
    isSndRcv (Write s) = False
    isSndRcv (Locked s) = False
    isSndRcv (RLocked s) = False
    isSndRcv t = True                 
                                                         


isInBarb :: String -> BarbContent -> Bool
isInBarb s (Send t) = s == t
isInBarb s (Rcv t) = s == t
isInBarb s (Read t) = s == t
isInBarb s (Write t) = s == t
isInBarb s (Lock t) = s == t
isInBarb s (Unlock t) = s == t
isInBarb s (RLock t) = s == t
isInBarb s (RUnlock t) = s == t
isInBarb s (Sel xs) = or $ map (isInBarb s) xs
isInBarb s t = False

oneLiveness :: Maybe String -> [String] -> (String, BarbContent) -> Maybe String
oneLiveness chan taus (f, b) = case chan of
  Nothing -> fun b f
  Just c -> if isInBarb c b then fun b f else Nothing
  where fun b f = case b of
          (Send s) -> Just $ sendrcv b s f
          (Rcv s) -> Just $ sendrcv b s f
          (Lock s) -> Just $ sendrcv b s f
          (RLock s) -> Just $ sendrcv b s f
          (Sel xs@(y:ts)) ->  Just $ "(<"++(printBarbs (f,b))++">true => "
                              ++(eventuallyHolds taus (helper xs))++")"
          t -> Nothing
  
        sendrcv t chan f =  "(<"++(printBarbs (f,t))++">true => "
                          ++(eventuallyHolds taus ("<Sync_"++chan++">true"))++")"
        printChan (Send s) = "(<Sync_"++s++">true)"
        printChan (Rcv s) = "(<Sync_"++s++">true)"
        helper xs = intercalate " || " $ map printChan xs


liveness :: Maybe String -> [String] -> [(String, BarbContent)] -> String
liveness chan taus xs =
  let list = catMaybes $ map (oneLiveness chan taus) xs
  in case list of
    [] -> "true % nothing to check here"
    ys -> "[("++(genTaus taus)++")*]\n(\n"
          ++(intercalate "\n&&\n" list)++"\n)"


oneDRF :: [(String, BarbContent)] -> (String, BarbContent) -> Maybe String
oneDRF barbs (f, b@(Write s)) = baddies >>= (\x -> return $ "(<"++(printBarbs (f, b))++">true => "++x)
  where baddies = case helper barbs f s of
          [] -> Nothing
          xs -> Just $ "[("++(intercalate " + " $ map printBarbs $ helper barbs f s)++")]false)"
        helper b f s = filter ((\x -> x /= f) . fst) (filter ((`elem` [Read s, Write s]) . snd) b)
oneDRF _ t = Nothing

drf :: [String] -> [(String, BarbContent)] -> Maybe String
drf taus xs = case catMaybes $ map (oneDRF xs) xs of
  [] -> Nothing -- "true % not write primitive"
  list -> Just $ "[("++(genTaus taus)++")*]\n(\n"++(intercalate "\n&&\n" list)++"\n)"


oneSafety :: [(String, BarbContent)] -> (String, BarbContent) ->  Maybe String
oneSafety barbs (f, b@(Closed s)) = baddies >>= (\x ->  return $ "(<"++(printBarbs (f, b))++">true => "++x++")")
  where baddies = case filter ((`elem` [Closing s]) . snd) barbs of
          [] -> Nothing
          xs -> Just $ "[("++(intercalate " + " $
                              map printBarbs $ filter ((`elem` [Send s, Closing s]) . snd) barbs)++")]false"
oneSafety barbs (f, b@(Unlock s)) = baddies >>= (\x ->  return $ "(<"++(printBarbs (f, b))++">true => "++x++")")
  where baddies = case filter ((`elem` [Locked s]) . snd) barbs of
          [] -> Nothing
          [xs] -> Just $ "<"++(printBarbs xs)++">true"
oneSafety barbs (f, b@(RUnlock s)) = baddies >>= (\x ->  return $ "(<"++(printBarbs (f, b))++">true => "++x++")")
  where baddies = case filter ((`elem` [RLocked s]) . snd) barbs of
          [] -> Nothing
          [xs] -> Just $ "<"++(printBarbs xs)++">true"
oneSafety _ t = Nothing



safety :: [String] -> [(String, BarbContent)] -> Maybe String
safety taus xs = case catMaybes $ map (oneSafety xs) xs of
  [] -> Nothing -- "true % not closing channel primitive"
  list -> Just $ "[("++(genTaus taus)++")*]\n(\n"++(intercalate "\n&&\n" list)++"\n)"





oneEventualReception :: [String] -> (String, BarbContent) -> Maybe String
oneEventualReception taus (f, b) =  fun b f
  where fun b f = case b of
          (NonEmpty s) -> Just $ sendrcv b s f
          t -> Nothing
        sendrcv t chan f =  "(<"++(printBarbs (f,t))++">true => mu X . <"
                          ++(genTaus taus)++">X || <Sync_"++chan++">true)"
  


eventualReception :: [String] -> [(String, BarbContent)] -> String
eventualReception taus xs =
  let list = catMaybes $ map (oneEventualReception taus) xs
  in case list of
    [] -> "true % nothing to check here"
    ys -> "[("++(genTaus taus)++")*]\n(\n"
          ++(intercalate "\n&&\n" list)++"\n)"
