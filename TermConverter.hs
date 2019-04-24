
{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , BangPatterns
  #-}
module TermConverter where

import GoParser
import Prelude hiding ((<>))

import Control.Applicative
import Control.Arrow ((+++))
import Control.Monad
import Control.Monad.Trans.Maybe
import Debug.Trace
import Data.Functor

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (render,(<+>),hsep,punctuate,brackets,(<>),text,Doc)



data IfTree a = ForNode (InitExpr a) (BoolExpr a) (StepExpr a) (IfTree a) (IfTree a)
--            | CallNode String 
--            | SpawnNode String 
            | SplitNode [IfTree a]
            | Bot
          deriving (Eq,Show,Ord)


                 
type IfForest a = [IfTree a]

type DefIfTree a = (String,IfTree a)

instance Functor IfTree where
  fmap f (ForNode init cond step b1 b2) = ForNode (fmap f init)
                                          (fmap f cond)
                                          (fmap f step)
                                          (fmap f b1)
                                          (fmap f b2)
--  fmap f (CallNode s ) = CallNode s                                           
--  fmap f (SpawnNode s ) = SpawnNode s 
  fmap f (SplitNode l) = SplitNode (fmap (fmap f) l)
  fmap f (Bot) = Bot
 -- fmap f (BranchNode l) = BranchNode (fmap (fmap f) l)


type DefEnv = M.Map String (Interm String)



subst ::  NumExprEnv -> IfTree String -> IfTree String
subst env (ForNode init cond step b1 b2) =
           ForNode (substInitExpr env init)
                   (substBoolExpr env cond)
                   (substStepExpr env step)
                   (subst env b1)
                   (subst env b2)
--subst env (CallNode s)  = CallNode s 
--subst env (SpawnNode s) = SpawnNode s
subst env (SplitNode l) = SplitNode (map (subst env) l)
subst env (Bot) = Bot

substForest :: NumExprEnv -> IfForest String -> IfForest String
substForest env = map (subst env)

flattenForest :: [IfForest a] -> IfForest a
flattenForest  = concat



collapseBots :: IfTree a -> IfTree a -> IfTree a
collapseBots t1@(ForNode init cond step b1 b2) t2 =
  ForNode init cond step (collapseBots b1 t2) (collapseBots b2 t2)
collapseBots t1@(SplitNode l) t2 = SplitNode (map (\t -> collapseBots t t2) l)
collapseBots Bot t2 = t2


interm2IfTree :: Interm String -> IfTree String
interm2IfTree (Seq l) = let l' = map interm2IfTree l in
                          foldr collapseBots Bot l'
interm2IfTree (Call s l1) = Bot
interm2IfTree (Spawn s l1) = Bot
interm2IfTree (Select l) = SplitNode (map interm2IfTree l)
interm2IfTree (If e t1 t2) = SplitNode [(interm2IfTree t1) ,
                                         (interm2IfTree t2)]
interm2IfTree (IfCond init cond step t1 t2) =
  ForNode init cond step (interm2IfTree t1) (interm2IfTree t2)
interm2IfTree t = Bot


        

                                

processDef :: Def (Interm String) -> DefIfTree String
processDef (D dName cParams body)  =
           (dName , interm2IfTree body)

sanitiseName s = map (\c -> if (c == '.') then '_' else
                              if (c == '$') then 'd' else
                                if (c == '#') then 'h' else c) s

sanitiseProg (Seq xs) = Seq (map sanitiseProg xs)
sanitiseProg (Call f args) = Call (sanitiseName f) args
sanitiseProg (Spawn f args) = Spawn (sanitiseName f) args
sanitiseProg (If e t t') = If e (sanitiseProg t) (sanitiseProg t')
sanitiseProg (IfCond i c s t1 t2) = IfCond i c s (sanitiseProg t1) (sanitiseProg t2)
sanitiseProg (Select xs) = Select (map sanitiseProg xs)
sanitiseProg t = t
         
                               

sanitiseDefNames defs = map sanitiseDef defs
 where sanitiseDef (D s args t) =
         let s' = sanitiseName s in D s' args (sanitiseProg t)
           

    

-- Transforms a program into a "termination structure", including
-- the main function.
processProg :: ProgGo -> [DefIfTree String]
processProg (P defs@(main:rest)) =
  let defs'@(main':rest') = sanitiseDefNames defs in -- makes names C compliant
             map processDef defs'


getMain :: [DefIfTree String] -> DefIfTree String
getMain = head


-- isRecursive s f
-- tests for occurrences of call/spawn of s in f
isRecursive :: String -> IfTree String -> Bool
isRecursive s (ForNode _ _ _ t1 t2) = isRecursive s t1 || isRecursive s t2
--isRecursive s (CallNode s' ) = (s == s')
--isRecursive s (SpawnNode s' ) = (s == s')
isRecursive s (SplitNode l) = or (map (isRecursive s) l)
isRecursive s Bot = False

-- has for loops
hasFor :: IfTree String -> Bool
hasFor (ForNode _ _ _ _ _) = True
--hasFor (CallNode _) = False
--hasFor (SpawnNode _) = False
hasFor (SplitNode l) = or (map hasFor l)
hasFor (Bot) = False


-- filterRecursive l produces a list l'
-- which consists exactly of the definitions in
-- l that are recursive.
filterRecursive :: [DefIfTree String] -> [DefIfTree String]
filterRecursive = filter (\(dName,f) -> isRecursive dName f)

filterFors :: [DefIfTree String] -> [DefIfTree String]
filterFors = filter (\(dname,f) -> hasFor f )


-- (1) All "paths" in a given IfTree --
allPaths :: IfTree String ->  [IfTree String]
allPaths (ForNode init cond step t1 t2) =
  let r1 = allPaths t1     -- :: [IfTree String]
      r2 = allPaths t2 in  -- :: [IfTree String]
   pathAux (init,cond,step) r1 r2
         
--allPaths t@(CallNode s) = [t] 
--allPaths t@(SpawnNode s)  = [t]
allPaths (SplitNode l)  =
 let r = map allPaths l in -- r :: [[IfTree String]]
     flattenForest r
allPaths (Bot)  = [Bot]

pathAux t@(init,cond,step) [x] [y] = [ForNode init cond step x y]
pathAux t@(init,cond,step) l1@(x:xs) l2@(y:ys) =
  (ForNode init cond step x y) :
  ((pathAux t [x] ys) ++ (pathAux t xs [y]) ++ pathAux t xs ys)
pathAux _ [] _ = []
pathAux _ _ [] = []
pathAux t@(init,cond,step) [x] l@(y:ys) =
  map (\t -> ForNode init cond step x t) l
pathAux t@(init,cond,step) l@(x1:xs) [y] =
  map (\t -> ForNode init cond step t y) l

-- all initalization variables in a tree --
allInits :: IfTree String -> [String]
allInits (ForNode init cond step t1 t2) =
  (getInitVar init):((allInits t1) ++ (allInits t2))  
allInits (SplitNode l) = map (concat . allInits) l
allInits _ = []

allfvars :: IfTree String -> [String]
allfvars (ForNode init cond step t1 t2) =
  let b = getInitVar init
      ivars = getInitFVars init
      cvars = getBExpVars cond
      svars = getStepVars step
      aux = nub (ivars ++ cvars ++ svars ++ (allfvars t1))   in
    nub ((delete b aux) ++ (allfvars t2))
allfvars (SplitNode l) = map (concat . allfvars) l 
allfvars _ = []

magic_prefix = "__"

-- (2) Expand a definition to "all possible paths" --
expandPaths :: DefIfTree String -> [(String,[String],IfTree String)]
expandPaths (dName,t) =
  let paths = allPaths t in -- paths :: [IfTree String]
  expandPaths' dName 0 paths
 where expandPaths' s n [] = []
       expandPaths' s n (p:ps) = ( (s ++ magic_prefix ++ (show n)) ,
                             (allfvars p) ,
                            p ):(expandPaths' s (n+1) ps)
                                 

-- (3) Print C Code ---

class CPrinter p where
  cprint :: p -> Doc

-- Numerical --
timesSym = text "*"
divSym = text "/"
addSym = text "+"
minusSym = text "-"
modSym = text "%"
incSym = text "++"
decSym = text "--"

-- Relations --
eq = text "=="
geq = text ">="
leq = text "<="
lte = text "<"
gte = text ">"
neq = text "!="

-- Logic --
andSym = text "&&"
orSym = text "||"
notSym = text "!"

-- Command --
assignSym = text "="

instance CPrinter (NumExpr String) where
 cprint (Num n) = text . show $ n
 cprint (Var s) = text s
 cprint (NBinop op n1 n2) = let n1' = cprint n1
                                op' = cprint op
                                n2' = cprint n2 in
                         PP.parens (n1' <+> op' <+> n2')
 cprint (NUnop op n) = let op' = cprint op
                           n'  = cprint n in
                     PP.parens (op' <> n')

instance CPrinter BEOpType where
 cprint And = andSym
 cprint Or = orSym

instance CPrinter BNOpType where
 cprint Eq = eq
 cprint GEq = geq
 cprint LEq = leq
 cprint LTE = lte
 cprint GTE = gte
 cprint NEq = neq

instance CPrinter BEUnOpType where
 cprint Not = notSym

instance CPrinter (InitExpr String) where
 cprint (Init n1 n2) = let n1' = cprint n1
                           n2' = cprint n2 in
                         n1' <+> assignSym <+> n2'
                          

instance CPrinter (StepExpr String) where
 cprint (UnStepOp op n) = let op' = cprint op
                              n' = cprint n in
                           n' <> op'
 cprint (Assign n1 n2) = let n1' = cprint n1
                             n2' = cprint n2 in
                          n1' <+> assignSym <+> n2'

instance CPrinter (UnOpType) where
 cprint Inc = incSym
 cprint Dec = decSym

instance CPrinter NOpType where
 cprint Times = timesSym
 cprint Div = divSym
 cprint Add = addSym
 cprint Minus = minusSym
 cprint Mod = modSym
 cprint Neg = minusSym

instance CPrinter (BoolExpr String) where
 cprint TT = text "true"
 cprint FF = text "false"
 cprint (BEBinop op b1 b2) = let op' = cprint op
                                 b1' = cprint b1
                                 b2' = cprint b2 in
                           PP.parens (b1' <+> op' <+> b2')
 cprint (BNBinop op n1 n2) = let op' = cprint op
                                 n1' = cprint n1
                                 n2' = cprint n2 in
                           PP.parens (n1' <+> op' <+> n2')
 cprint (BEUnary op b) = let op' = cprint op
                             b' = cprint b in
                          PP.parens (op' <> b')

genInitCode = render . cprint
genCondCode = render . cprint
genStepCode = render . cprint


-- This handles the breakdown of a function with more than
-- one for loop in sequence into multiple functions.
genExpansion :: [DefIfTree String] -> [(String,[String],IfTree String)]
genExpansion = genExpansion' 

genExpansion' = concat . map expandPaths

typesCommas [] = ""
typesCommas (x:[]) = "int " ++ x
typesCommas (x:xs) = "int " ++ x ++ ", " ++ typesCommas xs

genCCode' :: (String,[String],IfTree String) -> String
genCCode' (name,args,t) =
  "int "++name++"("++ typesCommas args  ++ ") { \n"
   ++ genBodyCode t ++ " \n" ++ "return 0; } \n"

genBodyCode :: IfTree String -> String
genBodyCode (ForNode init cond step t1 t2) =
  let body = genBodyCode t1 in
  "    for(int " ++  (genInitCode init) ++ "; " ++
      (genCondCode cond) ++ "; " ++ (genStepCode step) ++ ")" ++
      (if (body == "") then "; \n" else ("{ \n"
  ++ genBodyCode t1 ++ " } \n")) ++ genBodyCode t2
  

genBodyCode (SplitNode l) = error "genBodyCode: Badly formed."
--genBodyCode (CallNode s) = ""
--genBodyCode (SpawnNode s) = ""
genBodyCode Bot = ""

genMain :: [(String,[String],IfTree String)] -> (String,[String])
genMain l =
    let (body,rest) = genMain' l
        main = "int main() {\n" ++ body ++ "return 0; \n }" in
     (main,rest)
      
    

hasArgs ((_,[],_):xs) = hasArgs xs
hasArgs ((_,(a:as),_):xs) = True
hasArgs [] = False

genMain' ((dName,[],_):xs) =
  let (m,l) = genMain' xs in
   (dName ++ "();" ++ m , l)
genMain' ((dName,l@(y:ys),_):xs) =
  let (m,l) = genMain' xs in
  (m,dName:l)
genMain' [] = ("",[])

getMainCalls l =
  let (a,b,c) = unzip3 l in a
   
  
genCCode :: [(String,[String],IfTree String)] -> String
genCCode = foldr (\s acc -> (genCCode' s) ++ "\n" ++ acc) "" 


                     

