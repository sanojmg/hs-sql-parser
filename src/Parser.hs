module Parser where

import System.IO
import Control.Monad

import Text.Parsec.String (Parser)

import Text.ParserCombinators.Parsec hiding (try, (<|>))
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Lexer
import Type

import Text.Parsec (parse, try)
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, digit, string, anyChar, char, letter, satisfy, spaces)
import Text.Parsec.Combinator (many1, manyTill, eof, choice, between
                                     ,sepBy, optionMaybe, chainl1)
import Data.Char (isLetter, isDigit)

import Control.Applicative (many, (<*),(<$>), (*>), (<|>),(<$),(<*>))
import Control.Monad (void, guard)
import qualified Text.Parsec.Expr as E
import qualified Test.HUnit as H
import Data.Functor.Identity

import Debug.Trace (trace, traceM)


-- Parser a = Parsec String () a = ParsecT String () Identity a
-- s: stream, u: user state, m: monad, a: return type

qparse :: Parser a -> String -> Either ParseError a
qparse p = parse (whiteSpace *> p) ""

-- Terms in ArithExpr

integralLit :: Parser ArithExpr
integralLit = IntegralLit <$> integer 

fractionalLit :: Parser ArithExpr
fractionalLit = FractionalLit <$> float 

stringLit :: Parser ArithExpr
stringLit = StringLit <$> stringLiteral

identifierTrm :: Parser ArithExpr
identifierTrm = Identifier <$> identifier

qfdIdentifier :: Parser ArithExpr
qfdIdentifier = QfdIdentifier <$> identifier <*> (dot *> identifier)

asterisk :: Parser ArithExpr
asterisk = Asterisk <$ symbol "*"

qfdAsterisk :: Parser ArithExpr
qfdAsterisk = QfdAsterisk <$> (identifier <* dot <* symbol "*")

arithParens :: Parser ArithExpr
arithParens = ArithParens <$> (parens arithExpr)

arithCaseExpr :: Parser ArithExpr
arithCaseExpr = ArithCaseExpr <$> (caseExpr arithExpr)

caseExpr :: a -> Parser (CaseExpr a)
caseExpr resExpr = CaseExpr
                        <$> (reserved "case" *> many1 whenClause)
                        <*> optionMaybe elseClause
                        <*  reserved "end"
   where
      whenClause = (,) 
                       <$> (reserved "when" *> boolExpr)
                       <*> (reserved "then" *> resExpr)
      elseClause = reserved "else" *> resExpr

arithFunc :: Parser ArithExpr
arithFunc = ArithFunc 
                <$> identifier 
                <*> parens (commaSep expr)

boolParens :: Parser BoolExpr
boolParens = BoolParens <$> (parens boolExpr)

boolLit :: Parser BoolExpr
boolLit =  BoolLit True <$ reserved "true"
       <|> BoolLit False <$ reserved "false"
   
nullLit :: Parser BoolExpr
nullLit = NullLit <$ reserved "null"

boolCaseExpr :: Parser BoolExpr
boolCaseExpr = ArithCaseExpr <$> (caseExpr boolExpr)

boolFunc :: Parser ArithExpr
boolFunc = BoolFunc 
                <$> identifier 
                <*> parens (commaSep expr)

-- data ArithExpr = IntegralLit Integer
--                | FractionalLit Double
--                | StringLit String
--                | Identifier String
--                | QfdIdentifier String String
--                | Asterisk
--                | QfdAsterisk String
--                | Neg ArithExpr
--                | ArithBinary ArithBinOp ArithExpr ArithExpr 
--                | ArithParens ArithExpr
--                | ArithCaseExpr (CaseExpr ArithExpr) 
--                | ArithFunc FunctionName [Expr]  

-- type CasePredicate = BoolExpr
-- type CaseExpr a = [(CasePredicate, a)] (Maybe a)

arithTerm :: Parser ArithExpr
arithTerm =  try arithCaseExpr
         <|> try arithFunc
         <|> try qfdIdentifier
         <|> try qfdAsterisk
         <|> try fractionalLit
         <|> try integralLit
         <|> arithParens
         <|> stringLit
         <|> asterisk
         <|> identifierTrm

arithExpr :: Parser ArithExpr
arithExpr = buildExpressionParser arithOps arithTerm

relnExpr :: Parser BoolExpr
relnExpr = buildExpressionParser relnOps arithTerm

-- data BoolExpr = NullLit
--               | BoolLit Bool
--               | Not BoolExpr
--               | Exists SelectStmnt
--               | BoolBinary BoolBinOp BoolExpr BoolExpr
--               | RelBinary RelnBinOp ArithExpr ArithExpr
--               | BoolParens BoolExpr
--               | BoolCaseExpr (CaseExpr BoolExpr)
--               | BoolFunc FunctionName [Expr]
--                 deriving (Show) 

boolTerm :: Parser BoolExpr
boolTerm =  try boolCaseExpr
        <|> try boolFunc
        <|> nullLit
        <|> boolLit
        <|> boolParens
        <|> relnExpr

boolExpr :: Parser BoolExpr
boolExpr = buildExpressionParser boolOps boolTerm

-- data Expr = AExpr ArithExpr
--           | BExpr BoolExpr

expr :: Parser Expr
expr =  (AExpr <$> arithExpr)
    <|> (BExpr <$> boolExpr)

arithOps = [ [Prefix (reservedOp "-"   >> return (Neg                 ))          ]
           , [Infix  (reservedOp "*"   >> return (ArithBinary Multiply)) AssocLeft,
              Infix  (reservedOp "/"   >> return (ArithBinary Divide  )) AssocLeft,
              Infix  (reservedOp "%"   >> return (ArithBinary Modulo  )) AssocLeft]
           , [Infix  (reservedOp "+"   >> return (ArithBinary Add     )) AssocLeft,
              Infix  (reservedOp "-"   >> return (ArithBinary Subtract)) AssocLeft]
           ]


relnOps = [ [Infix  (reservedOp "<="   >> return (RelBinary LessOrEq   )) AssocRight,
             Infix  (reservedOp ">="   >> return (RelBinary GreaterOrEq)) AssocRight,
             Infix  (reservedOp "<"    >> return (RelBinary Less       )) AssocRight,
             Infix  (reservedOp ">"    >> return (RelBinary Greater    )) AssocRight,
             Infix  (reservedOp "like" >> return (RelBinary Like       )) AssocRight,
             Infix  (reservedOp "!="   >> return (RelBinary NotEq      )) AssocRight,
             Infix  (reservedOp "<>"   >> return (RelBinary NotEq      )) AssocRight,
             Infix  (reservedOp "="    >> return (RelBinary Eq         )) AssocRight]
          ]
    
boolOps = [ [Prefix (reservedOp "not"     >> return (Not            ))          ,
             Prefix (reservedOp "exists"  >> return (Exists         ))          ]
          , [Infix  (reservedOp "and"     >> return (BoolBinary And )) AssocLeft,
             Infix  (reservedOp "or"      >> return (BoolBinary Or  )) AssocLeft]
          ]

-- prefix :: String -> ArithExpr -> Operator String () Identity ArithExpr -- TODO
prefix name fun = Prefix (reservedOp name >> return fun)
postfix name fun = Postfix (reservedOp name >> return fun)
binary name fun assoc = Infix (reservedOp name >> return fun) assoc



