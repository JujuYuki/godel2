module FiniteControl where

import FlatGo
import GoParser
import Data.List as L
import qualified Data.Map as M


-- DEBUG
import System.IO.Unsafe
import Debug.Trace

checkFiniteControl :: ProgGo -> Bool
checkFiniteControl (P []) = True
checkFiniteControl prog@(P ((D t0 args t):ys)) = fun [t0] [] [] t
  where defs = mkMap prog
        fun seen lets spawns (Seq []) = True
        fun seen lets spawns (Seq (x:xs)) =
          case x of
          (Spawn f ys) -> (not $ f `elem` seen)
                          &&
                          (fun seen lets (f:spawns) (Seq xs))
                          &&
                          (fun (f:seen) [] [] (snd $ mylookup defs f))
          (NewChan chan s sy) -> fun seen (s:lets) spawns (Seq xs)
          (Call f ys) -> if f `elem` seen
                         then (null lets) && (null spawns)
                         else (fun (f:seen) [] [] (snd $ mylookup defs f))
                              &&
                              (fun seen lets spawns (Seq xs))
          (Seq ys) -> fun seen lets spawns (Seq (ys++xs))
          (If e t t') -> (fun seen lets spawns (Seq (t:xs)))
                         &&
                         (fun seen lets spawns (Seq (t':xs)))
          (IfCond e1 e2 e3 t t') -> (fun seen lets spawns (Seq (t:xs)))
                                    &&
                                    (fun seen lets spawns (Seq (t':xs)))
          (Select ys) -> and $ map (\t -> fun seen lets spawns (Seq (t:xs))) ys
          t -> fun seen lets spawns (Seq xs)
        fun seen lets spawns t = fun seen lets spawns (Seq [t])


mylookup defs f = case M.lookup f defs of
  Just s -> s
  Nothing -> error $ "Function not found: "++(show f)
                       
