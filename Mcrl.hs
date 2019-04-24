module Mcrl where

import Data.List as L
import qualified Data.Map as M
import Data.Char (toUpper)

data BarbContent = Sel [BarbContent] -- NB: xs in Sel[xs] is assumed to contain Send/Rcv only
                 | Rcv String
                 | Send String
                 | Read String
                 | Write String
                 | Lock String
                 | Unlock String
                 | RLock String
                 | RUnlock String
                 | Locked String
                 | RLocked String
                 | Closed String
                 | Closing String
                 | NonEmpty String
                 deriving (Eq, Show, Ord)

data Synchro = End String
             | Start String
             deriving (Eq, Show, Ord)

data Action = Barb BarbContent
            | Action String String
            | Request String
            deriving (Eq, Show, Ord)

data Model = Dot Model Model
           | Plus Model Model
           | A Action
           | Tau
           | Delta
           | Par Model Model
           | Rec String
           | Choice [(String, Model)]
           deriving (Eq, Show, Ord)



printBarbs :: (String, BarbContent) -> String
printBarbs (f, s) = case s of
  (Sel xs) -> helper xs
  (Rcv s) -> "RECV_"++s
  (Send s) -> "SEND_"++s
  (Read s) -> "READ_"++s
  (Write s) -> "WRITE_"++f++"_"++s
  (Lock s) -> "LOCK_"++s
  (Unlock s) -> "UNLOCK_"++s
  (RLock s) -> "RLOCK_"++s
  (RUnlock s) -> "RUNLOCK_"++s
  (Locked s) -> "LOCKED_"++s
  (RLocked s) -> "RLOCKED_"++s
  (Closing s) -> "CHANCLOSING_"++s
  (Closed s) -> "CHANCLOSED_"++s
  (NonEmpty s) -> "NONEMPTY_"++s
  where helper xs = foldl (++) ("SELECT_") $ map
                    (\x -> case x of
                        (Rcv chan) -> "R_"++chan
                        (Send chan) -> "S_"++chan
                    ) xs


-- (tau . delta + tau . delta) . (stuff) IS BAD, but this shouldn't happen anymore

printModel :: (String, Model) -> String
printModel (f, t) = fun (f, t)
  where fun (f, (Dot m1 m2)) = "("++(fun (f, m1))++") . ("++(fun (f, m2))++")"
        fun (f, (Plus m1 m2)) = "("++(fun (f, m1))++") + ("++(fun (f, m2))++")"
        fun (f, (A (Barb s))) = printBarbs (f, s)
        fun (f, (A (Action ty chan))) = ty++chan
        fun (f, (A (Request s))) = "REQ_"++s
        fun (f, Tau) = "tau"
        fun (f, Delta) = "delta"
        fun (f, (Par m1 m2)) = "("++(fun (f, m1))++")\n|| ("++(fun (f, m2))++")"
        fun (f, (Rec s)) = s
        fun (f, (Choice xs)) = intercalate " + " $ map helper xs
        helper (g,bod)
          | null g = (fun (f, bod))
          | otherwise = "\n("++g++") -> ("++(fun (f, bod))++")"



getActions :: Model -> [String]
getActions t = fun t
  where fun (A (Action ty chan)) = [ty++chan]
        fun (Par m1 m2) = (fun m1)++(fun m2)
        fun (Dot m1 m2) = (fun m1)++(fun m2)
        fun (Plus m1 m2) = (fun m1)++(fun m2)
        fun (Choice xs) = foldr (++) [] $ map (fun . snd) xs
        fun t = []

        
getGuards :: (String, Model) -> [(String, BarbContent)]
getGuards (f, t) = fun (f, t)
  where fun (f, (A (Barb s))) = helper f s 
        fun (f, (Par m1 m2)) = (fun (f, m1))++(fun (f, m2))
        fun (f, (Dot m1 m2)) = (fun (f, m1))++(fun (f, m2))
        fun (f, (Plus m1 m2)) = (fun (f, m1))++(fun (f, m2))
        fun (f, (Choice xs)) = foldr (++) [] $ map (\(x,y) -> (fun (f, y))) xs
        fun (f, t) = []
        helper f s = case s of
          (Write v) -> [(f, s)]
          _ -> [("_", s)]



