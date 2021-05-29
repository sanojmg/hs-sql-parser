{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type where

type ColumnAlias = Maybe String
type ColumnExpr = (Expr, ColumnAlias)
type SelectExpr = [ColumnExpr]

type TableName = String
type AliasName = String
type ColumnName = String
type FunctionName = String

type WhenPredicate = BoolExpr

data CaseExpr a = CaseExpr 
    { whenClause :: [(WhenPredicate, a)] 
    , elseClause :: (Maybe a)
    }
      deriving (Show)

data JoinType = InnerJoin 
              | LeftOuterJoin
              | RightOuterJoin
              | FullOuterJoin
                deriving (Eq, Show) 

data JoinCriteria = JoinOn BoolExpr
                  | JoinUsing [ColumnName]
                    deriving (Show)

data FromItem = TableRelation TableName
              | JoinRelation FromItem JoinType FromItem JoinCriteria
              | Subquery SelectStmnt 
              | FromParens FromItem
              | FromAlias FromItem AliasName
                deriving (Show)

type WherePredicate = BoolExpr
type ColumnList = [Expr]

data SelectStmnt = SelectStmnt 
    { selectExpr :: SelectExpr
    , fromItem :: FromItem
    , wherePred :: WherePredicate
    , groupBy :: ColumnList
    , orderBy :: ColumnList
    }
      deriving (Show)

data Expr = AExpr ArithExpr
          | BExpr BoolExpr
            deriving (Show)

data ArithExpr = IntegralLit Integer
               | FractionalLit Double
               | StringLit String
               | Identifier String
               | QfdIdentifier String String
               | Asterisk
               | QfdAsterisk String
               | Neg ArithExpr
               | ArithBinary ArithBinOp ArithExpr ArithExpr 
               | ArithParens ArithExpr
               | ArithCaseExpr (CaseExpr ArithExpr) 
               | ArithFunc FunctionName [Expr]  
                 deriving (Show)

data ArithBinOp = Add
                | Subtract
                | Multiply
                | Divide
                | Modulo
                  deriving (Show)

data BoolExpr = NullLit
              | BoolLit Bool
              | Not BoolExpr
              | Exists SelectStmnt
              | BoolBinary BoolBinOp BoolExpr BoolExpr
              | RelBinary RelnBinOp ArithExpr ArithExpr
              | BoolParens BoolExpr
              | BoolCaseExpr (CaseExpr BoolExpr)
              | BoolFunc FunctionName [Expr]
                deriving (Show) 

data RelnBinOp = LessOrEq
               | GreaterOrEq
               | Less
               | Greater
               | Like
               | NotEq
               | Eq
                 deriving (Show)

data BoolBinOp = And
               | Or
                 deriving (Show)

