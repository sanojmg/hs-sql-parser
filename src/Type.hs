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

type WhenPredicate = Expr

data CaseExpr = CaseExpr 
    { whenClause :: [(WhenPredicate, Expr)] 
    , elseClause :: (Maybe Expr)
    }
      deriving (Show)

data JoinType = InnerJoin 
              | LeftOuterJoin
              | RightOuterJoin
              | FullOuterJoin
                deriving (Eq, Show) 

data JoinCriteria = JoinOn Expr
                    deriving (Show)

data FromItem = FromItemJoin JoinRelation
              | FromItemNonJoin NonJoinRelation
                deriving (Show)

data NonJoinRelation = TableRelation TableName
                     | Subquery SelectStmnt 
                     | FromAlias FromItem AliasName
                       deriving (Show)

data JoinRelation = Join FromItem JoinType FromItem JoinCriteria
                    deriving (Show)

type WherePredicate = Expr
type ColumnList = [Expr]

data SelectStmnt = SelectStmnt 
    { selectCl :: SelectExpr
    , fromCl :: [FromItem]
    , whereCl :: Maybe WherePredicate
    , groupByCl :: ColumnList
    , orderByCl :: ColumnList
    }
    deriving (Show)


data Expr = IntegralLit Integer
          | FractionalLit Double
          | StringLit String
          | Identifier String
          | QfdIdentifier String String
          | Asterisk
          | QfdAsterisk String
          | Neg Expr
          | ArithBinary ArithBinOp Expr Expr 
          | Parens Expr
          | CaseSt CaseExpr
          | Func FunctionName [Expr]  
          | NullLit
          | BoolLit Bool
          | BIdentifier String
          | QfdBIdentifier String String
          | Not Expr
          | Exists SelectStmnt
          | BoolBinary BoolBinOp Expr Expr
          | RelBinary RelnBinOp Expr Expr
            deriving (Show) 

data ArithBinOp = Add
                | Subtract
                | Multiply
                | Divide
                | Modulo
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

