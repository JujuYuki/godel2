{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           
  #-}

module Printer where

import Control.Applicative
import Data.Functor
import Control.Arrow ((+++))
import Control.Monad
import Control.Monad.Trans.Maybe
--import Data.List as L
import Data.Set as S
import Data.Maybe

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (render,(<+>),hsep,punctuate,brackets,(<>),text,Doc)

import TermConverter
import GoParser


--- Pretty Printing ---

class Pretty p where
 ppr :: p -> Doc

--- Symbols ---
-- Logic --
andSym = text "&&"
orSym = text "||"
notSym = text "~"

-- Relations --
eq = text "="
geq = text ">="
leq = text "=<"
lte = text "<"
gte = text ">"
neq = text "<>"

-- Numerical --
timesSym = text "*"
divSym = text "/"
addSym = text "+"
minusSym = text "-"
modSym = text "%"



instance Pretty BEOpType where
 ppr And = andSym
 ppr Or = orSym

instance Pretty BNOpType where
 ppr Eq = eq
 ppr GEq = geq
 ppr LEq = leq
 ppr LTE = lte
 ppr GTE = gte
 ppr NEq = neq

instance Pretty BEUnOpType where
 ppr Not = notSym

instance Pretty NOpType where
 ppr Times = timesSym
 ppr Div = divSym
 ppr Add = addSym
 ppr Minus = minusSym
 ppr Mod = modSym
 ppr Neg = minusSym

instance Pretty (NumExpr String) where
 ppr (Num n) = text . show $ n
 ppr (Var x) = text x
 ppr (NBinop op n1 n2) = let n1' = ppr n1
                             op' = ppr op
                             n2' = ppr n2 in
                         PP.parens (n1' <+> op' <+> n2')
 ppr (NUnop op n) =  let op' = ppr op
                         n'  = ppr n in
                     PP.parens (op' <> n')

instance Pretty (BoolExpr String) where
 ppr TT = text "TRUE"
 ppr FF = text "FALSE"
 ppr (BEBinop op b1 b2) = let op' = ppr op
                              b1' = ppr b1
                              b2' = ppr b2 in
                           PP.parens (b1' <+> op' <+> b2')
 ppr (BNBinop op n1 n2) = let op' = ppr op
                              n1' = ppr n1
                              n2' = ppr n2 in
                           PP.parens (n1' <+> op' <+> n2')
 ppr (BEUnary op b) = let op' = ppr op
                          b' = ppr b in
                          PP.parens (op' <> b')


instance Pretty (IfTree String) where
 ppr (ForNode init step cond f1 f2) =
          let pf1 = punctuate PP.comma (fmap ppr f1)
              pf2 = punctuate PP.comma (fmap ppr f2) in
          text "ForNode" <+> PP.parens (ppr e) <+>
                PP.braces (hsep pf1) <+> PP.braces (hsep pf2)
 ppr (CallNode s params) =
          let ps = punctuate PP.comma (fmap ppr params) in
          text "CallNode" <+> text s <> PP.parens (hsep ps)
 ppr (SpawnNode s params) =
          let ps = punctuate PP.comma (fmap ppr params) in
          text "SpawnNode" <+> text s <> PP.parens (hsep ps)
 ppr (SelNode l) =
   let l' = fmap (\f -> PP.braces (hsep (punctuate PP.comma (fmap ppr
                                                            f)))) l
       l'' = punctuate PP.semi l'
       l''' = PP.braces (hsep l'') in
           text "SelNode" <+> l'''
 ppr Bot = text "Bot"


pprintIfTree :: IfTree String -> String
pprintIfTree t = render . ppr $ t

defIfTreePPrint :: DefIfTree String -> Doc
defIfTreePPrint (dName,eParams,f) =
      let pf = hsep (punctuate PP.semi (fmap ppr f))
          pars = hsep (punctuate PP.comma (fmap text eParams)) in
          text dName <> PP.parens pars <+> text "=" <+> pf

defIfProgPPrint :: [DefIfTree String] -> Doc
defIfProgPPrint l =
      let pf = punctuate PP.comma (fmap defIfTreePPrint l) in
       PP.braces (hsep pf)
      

pprintDefIfTree :: DefIfTree String -> String
pprintDefIfTree = render . defIfTreePPrint

pprintIfProg :: [DefIfTree String] -> String
pprintIfProg = render . defIfProgPPrint




--ifForest2TRS l = concat (map ifTree2TRS l)
  
