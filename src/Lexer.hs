{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Lexer where

import System.IO
import Control.Monad

import Text.Parsec.String (Parser)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "--"
           , Token.identStart      = letter <|> char '_'
           , Token.identLetter     = alphaNum <|> char '_'
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "like"
                                     , "exists"
                                     , "null"
                                     , "case"
                                     , "when"
                                     , "end"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     , "inner"
                                     , "outer"
                                     , "left"
                                     , "right"
                                     , "full"
                                     , "join"
                                     , "cross"
                                     , "natural"
                                     , "on"
                                     , "using"
                                     , "select"
                                     , "from"
                                     , "where"
                                     , "group"
                                     , "by"
                                     , "order"
                                     , "having"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "%", "<=", ">=", "<", ">" 
                                     , "like", "<>", "!=", "=",  "and", "or", "not", "exists"
                                     ]
           , Token.caseSensitive     = False
           }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer -- parses an identifier

reserved :: String -> Parser ()
reserved = Token.reserved lexer -- parses a reserved name

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer -- parses an operator

parens :: Parser a -> Parser a
parens = Token.parens lexer -- parens p parses p enclosed in parenthesis, returning the value of p.
                            
integer :: Parser Integer
integer = Token.integer lexer -- parses an integer

float :: Parser Double
float = Token.float lexer -- parses a floating point value. Returns the value of the number. 

semi :: Parser String
semi = Token.semi lexer -- parses a semicolon

comma :: Parser String -- comma parses the character ',' and skips any trailing white space. Returns the string ","
comma = Token.comma lexer 

commaSep :: forall a. Parser a -> Parser [a] -- commaSep p parses zero or more occurrences of p separated by comma. Returns a list of values returned by p.
commaSep = Token.commaSep lexer

commaSep1 :: forall a. Parser a -> Parser [a] -- commaSep1 p parses one or more occurrences of p separated by comma. Returns a list of values returned by p.
commaSep1 = Token.commaSep1 lexer

colon :: Parser String -- parses the character ':' and skips any trailing white space. Returns the string ":"
colon = Token.colon lexer 

dot :: Parser String -- parses the character '.' and skips any trailing white space. Returns the string "."
dot = Token.dot lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

symbol :: String -> Parser String   -- symbol s parses string s and skips trailing white space.
symbol = Token.symbol lexer

charLiteral :: Parser Char -- This lexeme parser parses a single literal character. Returns the literal character value.
charLiteral = Token.charLiteral lexer

stringLiteral :: Parser String -- parses a literal string. Returns the literal string value. 
stringLiteral = Token.stringLiteral lexer

lexeme :: forall a. Parser a -> Parser a -- lexeme p first applies parser p and then the whiteSpace parser, returning the value of p.
lexeme = Token.lexeme lexer

