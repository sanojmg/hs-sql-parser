{-# LANGUAGE OverloadedStrings #-}

module Parser where

import System.IO
import Control.Monad

import Text.Parsec.String (Parser)

import Text.ParserCombinators.Parsec hiding (many) 
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Lexer
import Type

import Text.Parsec (parse) 
import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)

import Control.Applicative (many, (<*), (<$>), (*>), (<$), (<*>)) 
import qualified Text.Parsec.Expr as E
import qualified Test.HUnit as H
import Data.Functor.Identity

import Debug.Trace (traceM)

-- Parser a = Parsec String () a = ParsecT String () Identity a
-- s: stream, u: user state, m: monad, a: return type
runParser :: String -> Either ParseError SelectStmnt
runParser = parse (whiteSpace *> selectStmnt) ""

runParserEof :: String -> Either ParseError SelectStmnt
runParserEof = parse (whiteSpace *> selectStmnt <* eof) ""

tparse :: Parser a -> String -> Either ParseError a
tparse p = parse (whiteSpace *> p) ""

tparseEof :: Parser a -> String -> Either ParseError a
tparseEof p = parse (whiteSpace *> p <* eof) ""

-- Terms in Expr

integralLit :: Parser Expr
integralLit = IntegralLit <$> integer 

fractionalLit :: Parser Expr
fractionalLit = FractionalLit <$> float 

stringLit :: Parser Expr
stringLit = StringLit <$> stringLiteral

identifierTrm :: Parser Expr
identifierTrm = Identifier <$> identifier

qfdIdentifier :: Parser Expr
qfdIdentifier = QfdIdentifier <$> identifier <*> (dot *> identifier)

asterisk :: Parser Expr
asterisk = Asterisk <$ symbol "*"

qfdAsterisk :: Parser Expr
qfdAsterisk = QfdAsterisk <$> (identifier <* dot <* symbol "*")

parensExpr :: Parser Expr
parensExpr = Parens <$> (parens expr)

caseExpr :: Parser Expr
caseExpr = CaseSt <$> ce
   where
      ce = CaseExpr
               <$> (reserved "case" *> many1 whenClause)
               <*> optionMaybe elseClause
               <*  reserved "end"
      whenClause = (,) 
                       <$> (reserved "when" *> expr)
                       <*> (reserved "then" *> expr)
      elseClause = reserved "else" *> expr
   
func :: Parser Expr
func = Func 
           <$> identifier 
           <*> parens (commaSep expr) 

boolLit :: Parser Expr
boolLit =  BoolLit True <$ reserved "true"
       <|> BoolLit False <$ reserved "false"
   
nullLit :: Parser Expr
nullLit = NullLit <$ reserved "null"

-- existExpr :: Parser BoolExpr
-- existExpr = existsOp <*> SelectStmnt

term :: Parser Expr
term =  caseExpr
    <|> try func
    <|> try qfdIdentifier
    <|> try qfdAsterisk
    <|> try fractionalLit
    <|> integralLit
    <|> parensExpr
    <|> stringLit
    <|> asterisk
    <|> identifierTrm
    <|> nullLit
    <|> boolLit
      --   <|> existExpr

expr :: Parser Expr
expr = buildExpressionParser (arithOps ++ relnOps ++ boolOps) term

-- arithOps :: [[Operator String () (Identity ArithExpr)]]
arithOps = [ [Prefix (reservedOp "-"   >> return (Neg                 ))          ]
           , [Infix  (reservedOp "*"   >> return (ArithBinary Multiply)) AssocLeft,
              Infix  (reservedOp "/"   >> return (ArithBinary Divide  )) AssocLeft,
              Infix  (reservedOp "%"   >> return (ArithBinary Modulo  )) AssocLeft]
           , [Infix  (reservedOp "+"   >> return (ArithBinary Add     )) AssocLeft,
              Infix  (reservedOp "-"   >> return (ArithBinary Subtract)) AssocLeft]
           ]

-- relnOps :: [[Operator String () (Identity BoolExpr)]]
relnOps = [ [Infix  (reservedOp "<="   >> return (RelBinary LessOrEq   )) AssocRight,
             Infix  (reservedOp ">="   >> return (RelBinary GreaterOrEq)) AssocRight,
             Infix  (reservedOp "<"    >> return (RelBinary Less       )) AssocRight,
             Infix  (reservedOp ">"    >> return (RelBinary Greater    )) AssocRight,
             Infix  (reserved   "like" >> return (RelBinary Like       )) AssocRight,
             Infix  (reservedOp "!="   >> return (RelBinary NotEq      )) AssocRight,
             Infix  (reservedOp "<>"   >> return (RelBinary NotEq      )) AssocRight,
             Infix  (reservedOp "="    >> return (RelBinary Eq         )) AssocRight]
          ]

existsOp = (reservedOp "exists"  >> return Exists)          
             
-- boolOps :: [[Operator String () (Identity BoolExpr)]]    
boolOps = [ [Prefix (reserved "not"     >> return (Not            ))          ]
          , [Infix  (reserved "and"     >> return (BoolBinary And )) AssocLeft,
             Infix  (reserved "or"      >> return (BoolBinary Or  )) AssocLeft]
          ]

-- prefix :: String -> ArithExpr -> Operator String () Identity ArithExpr -- TODO
prefix name fun = Prefix (reservedOp name >> return fun)
postfix name fun = Postfix (reservedOp name >> return fun)
binary name fun assoc = Infix (reservedOp name >> return fun) assoc

selectStmnt :: Parser SelectStmnt
selectStmnt = SelectStmnt
                  <$> selectExpr
                  <*> fromClause
                  <*> optionMaybe wherePred
                  <*> option [] groupBy
                  <*> option [] orderBy

selectExpr :: Parser SelectExpr
selectExpr = reserved "select" *> commaSep1 columnExpr
    where
      columnExpr = (,) <$> expr <*> columAlias
      columAlias = optionMaybe $ try aName
      aName = optional (reserved "as") *> identifier    

fromClause :: Parser [FromItem]
fromClause = (:)
               <$> (reserved "from" *> (FromItemNonJoin <$> nonJoinRelation))
               <*> option [] fromItmLst
       
fromItmLst :: Parser [FromItem]
fromItmLst = many (FromItemJoin <$> joinRelation) 

nonJoinRelation :: Parser NonJoinRelation
nonJoinRelation = do
    tab <- (subquery <|> tableRelation) 
    option tab (fromAlias tab)
  
tableRelation :: Parser NonJoinRelation
tableRelation = TableRelation <$> identifier

subquery :: Parser NonJoinRelation
subquery = Subquery <$> parens selectStmnt

fromAlias :: NonJoinRelation -> Parser NonJoinRelation
fromAlias reln = FromAlias reln <$> (reserved "as" *> identifier)

joinRelation :: Parser JoinRelation
joinRelation = Join <$> joinType 
                    <*> nonJoinRelation 
                    <*> joinCriteria

joinType :: Parser JoinType
joinType =  InnerJoin <$ optional (reserved "inner") <* reserved "join"
        <|> LeftOuterJoin <$ reserved "left" <* optional (reserved "outer") <* reserved "join" 
        <|> RightOuterJoin <$ reserved "right" <* optional (reserved "outer") <* reserved "join"
        <|> FullOuterJoin <$ reserved "full" <* optional (reserved "outer") <* reserved "join"

joinCriteria :: Parser JoinCriteria
joinCriteria =  JoinOn <$> (reserved "on" *> expr)

wherePred :: Parser WherePredicate
wherePred = reserved "where" *> expr

groupBy :: Parser ColumnList
groupBy = reserved "group" *> reserved "by" *> commaSep1 expr

orderBy :: Parser ColumnList
orderBy = reserved "order" *> reserved "by" *> commaSep1 expr

