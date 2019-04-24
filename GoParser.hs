{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}

module GoParser where

import Control.Arrow ((+++))
-- import qualified Data.Traversable
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Functor
import Data.List as L
import qualified Data.Map as M
-- import Data.Set as S
import Data.Maybe
import Control.Applicative ((<*),(*>))


import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>),many,(<?>))
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr

-- Types --

data Prog a = P [Def a]
            deriving (Eq, Show)
                     
data Def a =  D String [String] a
            deriving (Eq, Show)

instance Functor Def where
    fmap f (D s l p) = D s l (f p)

instance Functor Prog where
    fmap f (P l) = P (fmap (fmap f) l)

data InitExpr a = Init (NumExpr a) (NumExpr a) -- i := 0 / i := j
             deriving (Eq, Show, Ord)

substInitExpr :: NumExprEnv -> InitExpr String -> InitExpr String                      
substInitExpr env (Init n1 n2) = Init (substNumExpr env n1) (substNumExpr
                                                         env n2)
                             
getInitVar :: InitExpr a -> a
getInitVar (Init (Var v) _) = v
getInitVar _ = error "Bad Initialization."


getInitFVars (Init _ n) = getNExpVars n

instance Functor InitExpr where
  fmap f (Init n1 n2) = Init (fmap f n1) (fmap f n2)

data StepExpr a = UnStepOp UnOpType (NumExpr a) -- i++/--
                | Assign (NumExpr a) (NumExpr a) -- x := E
              deriving (Eq, Show, Ord)

getStepVars (UnStepOp op n) = getNExpVars n
getStepVars (Assign n1 n2) = (getNExpVars n1) ++ (getNExpVars n2)

substStepExpr :: NumExprEnv -> StepExpr String -> StepExpr String
substStepExpr env (UnStepOp op n) = UnStepOp op (substNumExpr env n)
substStepExpr env (Assign n1 n2) = Assign (substNumExpr env n1)
                                   (substNumExpr env n2)

data UnOpType = Inc | Dec
                  deriving (Eq, Show, Ord)

instance Functor StepExpr where
  fmap f (UnStepOp op e) = UnStepOp op (fmap f e)
  fmap f (Assign e1 e2) = Assign (fmap f e1) (fmap f e2)




data NumExpr a = Num Int
             | Var a
             | NBinop NOpType (NumExpr a) (NumExpr a)
             | NUnop NOpType (NumExpr a)
          deriving (Eq, Show, Ord)

type NumExprEnv = M.Map String (NumExpr String)

getNExpVars :: NumExpr a -> [a]
getNExpVars (Num _) = []
getNExpVars (Var a) = [a]
getNExpVars (NBinop op n1 n2) = (getNExpVars n1)++(getNExpVars n2)
getNExpVars (NUnop op n) = getNExpVars n


substNumExpr :: NumExprEnv -> NumExpr String ->  NumExpr String
substNumExpr env (Num n) = Num n
substNumExpr env (Var s) = case (M.lookup s env) of
                              Just a -> a
                              Nothing -> Var s
substNumExpr env (NBinop t e1 e2) = NBinop t (substNumExpr env e1)
                                    (substNumExpr env e2)
substNumExpr env (NUnop t e) = NUnop t (substNumExpr env e)                                    
                   

instance Functor NumExpr where
  fmap f (Num n) = Num n
  fmap f (Var s) = Var (f s)
  fmap f (NBinop t e1 e2) = NBinop t (fmap f e1) (fmap f e2)
  fmap f (NUnop t e) = NUnop t (fmap f e)

data NOpType = Times | Div | Add | Minus | Mod | Neg
          deriving (Eq, Show, Ord)

eLangDef = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = P.letter
              , identLetter = P.alphaNum
              , opStart = P.oneOf "~&=|</>+-*%"
              , opLetter = P.oneOf "~&=|</>+-*%"
              , reservedOpNames = ["~","&","=","|","<","<", ">", "<=",
                                   ">=", "!=","-","+","++","--","||","&&"
                                   ,"*","/","%"]
              , reservedNames = ["true","false"]
              }
T.TokenParser {T.parens = m_parens
            , T.identifier = m_identifier
            , T.reservedOp = m_reservedOp
            , T.reserved = m_reserved
            , T.semiSep1 = m_semiSep1
            , T.whiteSpace = m_whiteSpace } = T.makeTokenParser eLangDef


parseNumber :: P.Parser (NumExpr String)
parseNumber = liftM (Num . read) $ P.many1 P.digit

numExprParser :: P.Parser (NumExpr String)
numExprParser = buildExpressionParser numOpTable numTerm <?>
                "numerical expression"
boolExprParser :: P.Parser (BoolExpr String)
boolExprParser = buildExpressionParser boolOpTable boolTerm <?>
                 "boolean expression"

numOpTable = [[Prefix (m_reservedOp "-" >> return (NUnop Neg))]
        , [Infix (m_reservedOp "*" >> return (NBinop Times))
           AssocLeft]
        , [Infix (m_reservedOp "/" >> return (NBinop Div))
           AssocLeft]
        ,[Infix (m_reservedOp "+" >> return (NBinop Add))
           AssocLeft]
        ,[Infix (m_reservedOp "-" >> return (NBinop Minus))
           AssocLeft]
        ,[Infix (m_reservedOp "%" >> return (NBinop Mod))
           AssocLeft]]



boolOpTable = [[Prefix (m_reservedOp "!" >> return (BEUnary Not))]
              ,[Infix (m_reservedOp "&&" >> return (BEBinop And))
           AssocLeft]
              ,[Infix (m_reservedOp "||" >> return (BEBinop Or))
           AssocLeft]]


initTerm :: P.Parser (InitExpr String)
initTerm = do {
             m_whiteSpace 
           ; symbol "int"
           ; m_whiteSpace
           ; a1 <- numTerm
           ; symbol "="
           ; m_whiteSpace
           ; a2 <- numExprParser
           ; m_whiteSpace
           ; return (Init a1 a2) }
           
stepTerm :: P.Parser (StepExpr String)
stepTerm = do {
           m_whiteSpace
         ; a1 <- fmap Var m_identifier
         ; P.choice [(do { m_reservedOp "++" ; m_whiteSpace ; return (UnStepOp Inc a1) }),
                     (do {m_reservedOp "--" ; m_whiteSpace ; return (UnStepOp Dec a1) }),
                     (do {m_whiteSpace ; m_reservedOp "=" ; m_whiteSpace ; a2 <- numExprParser ; return (Assign a1 a2)})]
}



           
         

numTerm = m_parens numExprParser
          <|> parseNumber
          <|> fmap Var m_identifier

boolTerm = m_parens boolExprParser
          <|> (m_reserved "true" >> return TT)
          <|> (m_reserved "false" >> return FF)
          <|> relExprParser

relExprParser :: P.Parser (BoolExpr String)
relExprParser = do {
    m_whiteSpace
  ;  a1 <- numExprParser
  ;  m_whiteSpace
  ; op <- relation
  ; m_whiteSpace    
  ; a2 <- numExprParser
  ; m_whiteSpace
  ; return (BNBinop op a1 a2) }

relation :: P.Parser BNOpType
relation = (m_reservedOp "==" >> return Eq)
        <|> (m_reservedOp ">=" >> return GEq)
        <|> (m_reservedOp "<=" >> return LEq)           
        <|> (m_reservedOp ">" >> return GTE) 
        <|> (m_reservedOp "<" >> return LTE)
        <|> (m_reservedOp "!=" >> return NEq)          

data BoolExpr a = TT | FF
              | BEBinop BEOpType (BoolExpr a) (BoolExpr a)
              | BNBinop BNOpType (NumExpr a) (NumExpr a)
              | BEUnary BEUnOpType (BoolExpr a)
          deriving (Eq, Show, Ord)

getBExpVars :: BoolExpr a -> [a]
getBExpVars TT = []
getBExpVars FF = []
getBExpVars (BEBinop op b1 b2) = (getBExpVars b1)++(getBExpVars b2)
getBExpVars (BNBinop op n1 n2) = (getNExpVars n1)++(getNExpVars n2)
getBExpVars (BEUnary op b) = getBExpVars b

data BEOpType = And | Or 
          deriving (Eq,Show,Ord)

data BNOpType = Eq | GEq | LEq | LTE | GTE | NEq
          deriving (Eq,Show,Ord)

data BEUnOpType = Not
          deriving (Eq,Show,Ord)
                   
substBoolExpr ::  NumExprEnv -> BoolExpr String  -> BoolExpr String
substBoolExpr env TT = TT
substBoolExpr env FF = FF
substBoolExpr env (BEBinop t e1 e2)  = BEBinop t (substBoolExpr env e1)
                                      (substBoolExpr env e2)
substBoolExpr env (BNBinop t n1 n2)  = BNBinop t (substNumExpr env n1)
                                      (substNumExpr env n2)
substBoolExpr env (BEUnary t e)  = BEUnary t (substBoolExpr env e)

instance Functor BoolExpr where
  fmap f TT = TT
  fmap f FF = FF
  fmap f (BEBinop t b1 b2) = BEBinop t (fmap f b1) (fmap f b2)
  fmap f (BNBinop t e1 e2) = BNBinop t (fmap f e1) (fmap f e2)
  fmap f (BEUnary t b) = BEUnary t (fmap f b)




                   



data Interm a = Seq [Interm a]
              | Call String [a] 
              | Cl a
              | Spawn String [a] 
              | NewChan a String Integer
              | NewVar a
              | NewSync a String
              | If (BoolExpr a) (Interm a) (Interm a)
              | IfCond (InitExpr a) (BoolExpr a) (StepExpr a) (Interm a) (Interm a)
              | Select [Interm a]
              | T -- tau
              | S a
              | R a
              | St a
              | Ld a
              | Lck a
              | Ulck a
              | RLck a
              | RUlck a
              | Zero
         deriving (Eq, Show, Ord)


instance Functor Interm where
  fmap f (Seq xs) = Seq (map (fmap f) xs)
  fmap f (Call fun xs) = Call fun (map f xs) 
  fmap f (Cl chan) = Cl (f chan)
  fmap f (Spawn fun xs ) = Spawn fun (map f xs) 
  fmap f (NewChan chan s sy) = NewChan (f chan) s sy
  fmap f (NewVar var) = NewVar (f var)
  fmap f (NewSync s t) = NewSync (f s) t
  fmap f (If e t t') = If (fmap f e) (fmap f t) (fmap f t')
  fmap f (IfCond i c s t1 t2) = IfCond (fmap f i)
                                       (fmap f c)
                                       (fmap f s)
                                       (fmap f t1)
                                       (fmap f t2)
  fmap f (Select xs) = Select (map (fmap f) xs)
  fmap f T = T
  fmap f (S chan) = S (f chan)
  fmap f (R chan) = R (f chan)
  fmap f (Ld var) = Ld (f var)
  fmap f (St var) = St (f var)
  fmap f (Lck mut) = Lck (f mut)
  fmap f (Ulck mut) = Ulck (f mut)
  fmap f (RLck mut) = RLck (f mut)
  fmap f (RUlck mut) = RUlck (f mut)
  fmap f Zero = Zero
  




type ProgGo = Prog (Interm String)
type DefGo = Def (Interm String)



-- Lexer --
lexer :: T.TokenParser ()

ldef = emptyDef {  T.identStart = P.letter
                 , T.identLetter = (P.alphaNum <|> P.char '_' <|> P.char '.' 
                                               <|> P.char '$'<|> P.char '#')
                 , T.reservedNames = [ "def"
                                     , "call"
                                     , "close"
                                     , "spawn"
                                     , "let"
                                     , "letmem"
                                     , "newsync"
                                     , "newchan"
                                     , "select"
                                     , "case"
                                     , "endselect"
                                     , "if"
                                     , "then"
                                     , "ifFor"
                                     , "else"
                                     , "endif"
                                     , "tau"
                                     , "send"
                                     , "recv"
                                     , "read"
                                     , "write"
                                     , "lock"
                                     , "unlock"
                                     , "rlock"
                                     , "runlock" ]
                 , T.commentLine = "--"
                 }
 
lexer = T.makeTokenParser ldef

whiteSpace = T.whiteSpace lexer
reserved   = T.reserved lexer
parens     = T.parens lexer
identifier = T.identifier lexer
natural    = T.natural lexer
integer    = T.integer lexer
semi       = T.semi lexer
symbol     = T.symbol lexer

-- Parser --

seqInterm :: P.Parser (Interm String)
seqInterm = do
  list <- P.sepBy1 itparser semi
  return $ if L.length list == 1 then head list else Seq list

pparser :: P.Parser (ProgGo)
pparser = do
  l <- many dparser
  return $ P l


dparser :: P.Parser (DefGo)
dparser = do
  { reserved "def"
  ; x <- identifier
  --; symbol "("
  ; l1 <- parens (P.sepBy identifier (P.char ',' <* P.spaces))
  --; l2 <- P.option [] (do { symbol ";" ; P.sepBy identifier (P.char ',' <* P.spaces)  })
  --; symbol ")"
  ; symbol ":"
  ; d <- seqInterm
  ; return $ D x l1 d
  }


itparser :: P.Parser (Interm String)
itparser = 
  do { reserved  "close"
     ; c <- identifier
     ; return $ (Cl c) }
  <|>
  do { reserved "spawn"
     ; x <- identifier
--     ; symbol "("
     ; l1 <- parens (P.sepBy identifier (P.char ',' <* P.spaces))
--     ; l2 <- P.option [] (do { symbol ";" ; P.sepBy numExprParser (P.char ',' <* P.spaces)  })
--     ; symbol ")"
     ; return $ Spawn x l1 }
  <|>
  do { reserved "select"
     ; l <- many (reserved "case" *> seqInterm)
     ; reserved "endselect"
     ; return $ Select l }
  <|>
  do { reserved "let"
     ; x <- identifier
     ; symbol "="
     ; reserved "newchan"
     ; t <- identifier
     ; symbol ","
     ; n <- natural
     ; return $ NewChan x t n }
  <|>
  do { reserved "letmem"
     ; x <- identifier
     ; return $ NewVar x }
  <|>
  do { reserved "letsync"
     ; x <- identifier
     ; y <- identifier
     ; return $ NewSync x y }
  <|>
  do { reserved "if"
     ; t <- seqInterm
     ; reserved "else"
     ; e <- seqInterm
     ; reserved "endif"
     ; return $ If TT t e }
  <|>
  do { reserved "ifFor"
     ; symbol "("
     ; init <- initTerm
     ; symbol ";"
     ; cond <- boolExprParser
     ; symbol ";"
     ; step <- stepTerm
     ; symbol ")"
     ; reserved "then"
     ; t1 <- seqInterm
     ; reserved "else"
     ; t2 <- seqInterm
     ; reserved "endif"
     ; return $ IfCond init cond step t1 t2 }
  <|>
  do { reserved "tau"
     ; return $ T   }
  <|>
  do { reserved "send"
     ; c <- identifier
     ; return $ S c  }
  <|>
  do { reserved "recv"
     ; c <- identifier
     ; return $  R c  }
  <|>
  do { reserved "read"
     ; v <- identifier
     ; return $ Ld v }
  <|>
  do { reserved "write"
     ; v <- identifier
     ; return $ St v }
  <|>
  do { reserved "lock"
     ; m <- identifier
     ; return $ Lck m }
  <|>
  do { reserved "unlock"
     ; m <- identifier
     ; return $ Ulck m }
  <|>
  do { reserved "rlock"
     ; m <- identifier
     ; return $ RLck m }
  <|>
  do { reserved "runlock"
     ; m <- identifier
     ; return $ RUlck m }
  <|>
  do  { reserved "call"
  ; c <- identifier
--  ; symbol "("
  ; l1 <- parens (P.sepBy identifier (P.char ',' <* P.spaces))
--  ; l2 <- P.option [] (do { symbol ";" ; P.sepBy numExprParser (P.char ',' <* P.spaces)  })        
--  ; symbol ")"
  ;  return $ Call c l1  }
 <|>
  do { return $ Zero }

mainparser :: P.Parser (ProgGo)
mainparser = whiteSpace >> pparser <* P.eof


parseprog :: String -> Either P.ParseError (ProgGo)
parseprog inp = P.parse mainparser "" inp

parseBoolExpr :: String -> Either P.ParseError (BoolExpr String)
parseBoolExpr inp = P.parse boolExprParser "" inp

parseRelExpr :: String -> Either P.ParseError (BoolExpr String)
parseRelExpr inp = P.parse relExprParser "" inp

parseTest s =
  case parseprog s of
  Left err -> print err
  Right s -> print s

parseBoolTest s =
  case (parseBoolExpr s) of
  Left err -> print err
  Right s -> print s

parseRelTest s =
  case (parseRelExpr s) of
  Left err -> print err
  Right s -> print s
