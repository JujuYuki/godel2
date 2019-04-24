module FlatGo where

import GoParser
import Data.List as L
import qualified Data.Map as M

-- DEBUG
import System.IO.Unsafe
import Debug.Trace

type Envi = M.Map String String
type FunDefs = M.Map String ([String], Interm String)
type ProcDefs = M.Map String (Interm String)

mkMap :: ProgGo -> FunDefs
mkMap (P xs) = M.fromList $ map (\(D fun args t) -> (fun, (args,t))) xs



rename :: Envi -> String -> Interm String -> Interm String
rename env pref t = snd $ renameChans pref env t

renameDef :: DefGo -> DefGo
renameDef (D fun args t) =
  let env = M.fromList $ map (\x -> (x,fun++x)) args
  in D fun (map ((++) fun) args) (rename env fun t)

renameProg :: ProgGo -> ProgGo
renameProg (P xs) = P $ map renameDef xs




replaceChan :: Envi -> String -> String
replaceChan map a = case M.lookup a map of
  Just a' -> a'
  Nothing -> a -- error $ "Channel "++a++" is not declared."



mkSubstitution :: String -> [String] -> [String] -> (String -> String)
mkSubstitution fun formal actual =
  let mm = M.fromList $ zip formal actual
  in \x -> case M.lookup x mm of
    Just a -> a
    Nothing -> x


mkFunName :: String -> [String] -> String
mkFunName fun xs  = fun++(foldr (++) "" xs)

duplicateFunctions :: [String] -> FunDefs -> Interm String -> [(String, Interm String)]
duplicateFunctions visited env (Call fun xs) = case M.lookup fun env of
  Just (args,t) -> let instt = instantiate (mkSubstitution fun args xs) t
                       nl = if (mkFunName fun xs) `elem` visited
                            then []
                            else duplicateFunctions ((mkFunName fun xs):visited) env instt
                   in [(mkFunName fun xs, instt)]++nl
  Nothing -> error $ "Function "++fun++" is not declared."
duplicateFunctions visited env (Spawn fun xs) = case M.lookup fun env of
  Just (args,t) -> let instt = instantiate (mkSubstitution fun args xs) t
                       nl = if (mkFunName fun xs) `elem` visited
                            then []
                            else duplicateFunctions ((mkFunName fun xs):visited) env instt
                   in [(mkFunName fun xs, instt)]++nl
  Nothing -> error $ "Function "++fun++" is not declared."
duplicateFunctions visited env (Seq xs) = foldr (++) [] $ map (duplicateFunctions visited env) xs
duplicateFunctions visited env (Select xs) = foldr (++) [] $ map (duplicateFunctions visited env) xs
duplicateFunctions visited env (If e t t') =
  (duplicateFunctions visited env t)++(duplicateFunctions visited env t')
duplicateFunctions visited env (IfCond e1 e2 e3 t t') =
  (duplicateFunctions visited env t)++(duplicateFunctions visited env t')
duplicateFunctions visited env t = []




renameCalls ::  Interm String ->  Interm String
renameCalls (Spawn fun xs) = Spawn (mkFunName fun xs) []
renameCalls (Call fun xs) = Call (mkFunName fun xs) []
renameCalls (If e t t') = If e (renameCalls t) (renameCalls t')
renameCalls (IfCond e1 e2 e3 t t') = IfCond e1 e2 e3 (renameCalls t) (renameCalls t')
renameCalls (Seq xs) = Seq $ map renameCalls xs
renameCalls (Select xs) = Select $ map renameCalls xs
renameCalls t = t



flattenProgGo :: ProgGo -> ProgGo
flattenProgGo (P []) = error $ "No function defined."
flattenProgGo prog@(P ((D fun args t):xs)) =
  let funlist = nub $ duplicateFunctions [] (mkMap prog) t
      recalls = map (\(x,y) -> (x, renameCalls y)) ((fun,t):funlist)
  in P $ map (\(x,y) -> ((D x [] y))) recalls


renameChans :: String -> Envi -> Interm String -> (Envi, Interm String)
renameChans pref sub (Seq [x]) = renameChans pref sub x
renameChans pref sub (Seq xs) =
  let (env', ty) = 
        mapAccumL (\map t -> let (env, ty) = renameChans pref map t
                             in (M.union env map, ty)
                  ) sub xs
  in (env', Seq ty)
renameChans pref sub (Call fun xs ) = (sub, Call fun (map (replaceChan sub) xs))
renameChans pref sub (Cl chan) = (sub, Cl (replaceChan sub chan))
renameChans pref sub (Spawn fun xs) = (sub, Spawn fun (map (replaceChan sub) xs))
renameChans pref sub (NewChan chan s sy) = (M.insert chan (pref++chan) sub,  NewChan (pref++chan) s sy)
renameChans pref sub (NewVar var) = (M.insert var (pref++var) sub, NewVar (pref++var))
renameChans pref sub (NewSync mut t) = (M.insert mut (pref++mut) sub, NewSync (pref++mut) t)
renameChans pref sub (If e t t') = let (nsub, nt) = renameChans (pref++"L") sub t
                                       (nsub', nt') = renameChans (pref++"R") sub t'
                                   in (M.union nsub nsub', If e nt nt')
renameChans pref sub (IfCond e1 e2 e3 t t') = let (nsub, nt) = renameChans (pref++"L") sub t
                                                  (nsub', nt') = renameChans (pref++"R") sub t'
                                              in (M.union nsub nsub', IfCond e1 e2 e3 nt nt')
renameChans pref sub (Select xs) = let ll = snd $
                                            mapAccumL (\i x -> ((i+1), renameChans (pref++(show i)) sub x)) 0 xs
                                   in (M.unions $ map fst ll, Select $ map snd ll)
renameChans pref sub (S chan) = (sub, S $ replaceChan sub chan)
renameChans pref sub (R chan) = (sub, R $ replaceChan sub chan)
renameChans pref sub Zero = (sub, Zero)
renameChans pref sub (St var) = (sub, St $ replaceChan sub var)
renameChans pref sub (Ld var) = (sub, Ld $ replaceChan sub var)
renameChans pref sub (Lck mut) = (sub, Lck $ replaceChan sub mut)
renameChans pref sub (Ulck mut) = (sub, Ulck $ replaceChan sub mut)
renameChans pref sub (RLck mut) = (sub, RLck $ replaceChan sub mut)
renameChans pref sub (RUlck mut) = (sub, RUlck $ replaceChan sub mut)
renameChans pref sub T = (sub, T)


instantiate :: (String -> String) -> Interm String -> Interm String
instantiate f t = fmap f t


dingoHunterRename :: String -> String
dingoHunterRename = map (\x -> if x `elem` ['$','#','.']
                               then '_'
                               else x)


fixNames :: ProgGo -> ProgGo
fixNames (P fs) = P $ map helper fs
  where helper (D s xs t) = D (dingoHunterRename s) xs (fmap dingoHunterRename $ rem t)
        rem (Spawn fun xs) = Spawn (dingoHunterRename fun) xs
        rem (Call fun xs) = Call (dingoHunterRename fun) xs
        rem (If e t t') = If e (rem t) (rem t')
        rem (IfCond e1 e2 e3 t t') = IfCond e1 e2 e3 (rem t) (rem t')
        rem (Select xs) = Select $ map rem xs
        rem (Seq xs) = Seq $ map rem xs
        rem t = t



sequentialiseProg:: ProgGo -> ProgGo
sequentialiseProg (P fs) = P $ map  (\(D fun args t) -> D fun args (sequentialise t)) fs

sequentialise :: Interm String -> Interm String
sequentialise t = helper t
  where helper (Seq (x:xs)) = case x of
          (If e t t') -> If e (Seq (t:xs)) (Seq (t':xs))
          (IfCond e1 e2 e3 t t') -> IfCond e1 e2 e3 (Seq (t:xs)) (Seq (t':xs))
          (Select ys) -> Select $ map (\t' -> Seq (t':xs)) ys
          t -> Seq [helper t, helper (Seq xs)]
        helper (Seq []) = Zero
        helper (If e t t') = If e (helper t) (helper t')
        helper (IfCond e1 e2 e3 t t') = IfCond e1 e2 e3 (helper t) (helper t')
        helper (Select xs) =  Select (map helper xs)
        helper t = t
        

-- UNUSED FOR THE MOMENT
-- Not sure how to deal with, e.g., t0 = (nu a b) f(a); g(b); 0   with f(x) = x;0 and g(y) = y; 0
seqCompToPrefix :: ProgGo -> ProgGo
seqCompToPrefix p = P $ map (\(k,(p,t)) -> D k p t) $ M.toList (removeCallSequences $ mkMap p)

removeCallSequences :: FunDefs -> FunDefs
removeCallSequences odefs = let ndefs = helper odefs (M.toList odefs)
                            in if (M.size ndefs /= M.size odefs) 
                               then removeCallSequences ndefs
                               else ndefs
  where helper defs ((k,(p,t)):xs) = let (ndefs, t') = analyse defs t
                                     in helper (M.insert k (p,t') ndefs) xs
        helper defs [] = defs
        analyse defs (If e t1 t2) = let (d1, t1') = analyse defs t1
                                        (d2, t2') = analyse defs t2
                                    in (M.union d1 d2, If e t1' t2')
        analyse defs (IfCond e1 e2 e3 t1 t2) = let (d1, t1') = analyse defs t1
                                                   (d2, t2') = analyse defs t2
                                          in (M.union d1 d2, IfCond e1 e2 e3 t1' t2')                          
        analyse defs (Select xs) = let ys = map (analyse defs) xs
                                   in (M.unions $ map fst ys, Select $ map snd ys)
        analyse defs (Seq []) = (defs, Zero)
        analyse defs (Seq [x]) = let (ndefs, t') = analyse defs x
                                 in (ndefs, t')
        analyse defs (Seq (x:y:xs)) = case x of 
          (Call m ps) -> let (ndefs, t') = pushCallIn (Seq (y:xs)) defs m (snd $ defs M.! m)
                         in (M.insert (m++"NEW") (fst $ defs M.! m, t') ndefs, Call (m++"NEW") ps)
          _ -> let (ndefs, t') = analyse defs (Seq (y:xs))
               in (ndefs, Seq [x,t'])
        analyse defs t' = (defs, t')
        
-- Call this *after* sequentialise
pushCallIn :: Interm String -> FunDefs -> String -> Interm String -> (FunDefs, Interm String)
pushCallIn g defs n t = helper t
  where  helper (Zero) = (M.empty, g)
         helper (If e t1 t2) = let (d1,t1') = helper t1
                                   (d2,t2') = helper t2
                               in (M.union d1 d2, If e t1' t2')
         helper (IfCond e1 e2 e3 t1 t2) = let (d1,t1') = helper t1
                                              (d2,t2') = helper t2
                                          in (M.union d1 d2, IfCond e1 e2 e3 t1' t2')
         helper (Select xs) = let ys = map helper xs
                              in (M.unions $ map fst ys, Select $ map snd ys)
         helper (Call m ps) = if m==n 
                              then (defs, Call m ps)
                              else let (ndefs, t') = pushCallIn g defs m (snd $ defs M.! m)             
                                   in (M.insert (m++"EXTRA") (fst $ defs M.! m, t') ndefs, Call (m++"EXTRA") ps)
         helper (Seq [x]) = helper x
         helper (Seq xs) = (defs, Seq $ xs++[g])
         helper t = (defs, t)
