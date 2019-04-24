module DeadcodeAnalysis where

import GoParser 
import FlatGo
import Data.List as L
import Data.Maybe
import qualified Data.Map as M


-- DEBUG
import System.IO.Unsafe
import Debug.Trace


isDead :: Bool -> FunDefs -> [String] -> Interm String -> Bool
isDead flag defs seen Zero = True
isDead flag defs seen T = True
isDead flag defs seen (Seq []) = True
isDead flag defs seen (Seq xs) = null $ filter (not . isDead flag defs seen) xs
isDead flag defs seen (If e t t') = (isDead flag defs seen t) && (isDead flag defs seen t')
isDead flag defs seen (IfCond _ _ _ t t') = (isDead flag defs seen t) && (isDead flag defs seen t')
isDead flag defs seen (Call f ps) 
  | ((length seen) > 10) && ((length seen) > (M.size defs) `div` 50) = False
  | flag = if f `elem` seen
           then True
           else isDead flag defs (seen++[f]) $ case M.lookup f defs of
             Just (ps, t) -> t
             Nothing -> error $ "[isDead] Function "++f++" not found!"
  | otherwise = False
isDead flag defs seen t = False


deadDefinitions :: Bool -> ProgGo -> [String]
deadDefinitions flag (P xs) = 
  let defs = mkMap (P xs)
  in map (\(D name args body) -> name) $ filter (\(D name args body) -> 
                                                  (isDead flag defs [name] body) ) (tail xs)


replaceDeadcode :: ProgGo -> [String] -> ProgGo
replaceDeadcode (P xs) zombies = P $ map (\(D name args body) -> D name args (fun body)) xs
  where fun t@(Spawn f xs) = if f `elem` zombies
                             then Zero
                             else t
        fun t@(Call f xs) = if (f `elem` zombies)
                            then Zero
                            else t
        fun (Seq []) = Zero
        fun (Seq [x]) = fun x
        fun (Seq xs) = Seq (map fun xs)
        fun (If e t t') = If e (fun t) (fun t')
        fun (IfCond e1 e2 e3 t t') = IfCond e1 e2 e3 (fun t) (fun t') 
        fun (Select xs) = Select (map fun xs)
        fun t = t
        defs = mkMap (P xs)



filterDeaddefinitions :: ProgGo -> [String] -> ProgGo
filterDeaddefinitions (P xs) zombies =
  P $ filter (\(D name args body) -> not (name `elem` zombies)) xs

removeDeadcode :: Bool -> ProgGo -> ProgGo
removeDeadcode flag prog@(P xs) = let zombies = deadDefinitions flag prog
                                      nprog = replaceDeadcode prog zombies
                                  in if null zombies
                                     then prog
                                     else removeDeadcode flag (filterDeaddefinitions nprog zombies)



