module Pretty where

import qualified Data.Text as T

import Prettyprinter
import Prettyprinter.Util

import Type
import Parser (tparseEof)
import Text.Parsec.String (Parser)

-- class Pretty a where
--     pretty a :: a -> Doc

pprint :: Pretty a => Parser a -> String -> Doc ann
pprint p s = case (tparseEof p s) of
    Right t -> pretty t
    Left e -> pretty $ "Error parsing: " ++ show e


instance Pretty SelectStmnt where
    pretty (SelectStmnt selectCl fromCl whereCl groupByCl orderByCl) = undefined

instance Pretty JoinType where
    pretty InnerJoin = prettyS "INNER JOIN" 
    pretty LeftOuterJoin = prettyS "LEFT OUTER JOIN"  
    pretty RightOuterJoin = prettyS "RIGHT OUTER JOIN"  
    pretty FullOuterJoin = prettyS "FULL OUTER JOIN" 

instance Pretty Expr where
    pretty (IntegralLit n) = pretty n
    pretty (FractionalLit d) = pretty d
    pretty (StringLit s) = squotes $ pretty s
    pretty (Identifier s) = pretty s
    pretty (QfdIdentifier t c) = pretty t <> dot <> pretty c
    pretty Asterisk = prettyS "*"
    pretty (QfdAsterisk t) = pretty t <> dot <> prettyS "*"
    pretty (Neg e) = prettyS "-" <+> pretty e
    pretty (ArithBinary op e1 e2) = fillSep [pretty e1, pretty op, pretty e2]
    pretty (Parens e) = parens $ pretty e
    pretty (CaseSt ce) = pretty ce
    pretty (Func f el) = pretty f <> (parens $ fillSep $ punctuate comma $ map pretty el) 
    pretty NullLit = prettyS "NULL"
    pretty (BoolLit b) = pretty b
    pretty (Not e) = prettyS "NOT" <+> pretty e
    pretty (Exists e) = prettyS "EXISTS" <+> pretty e
    pretty (BoolBinary op e1 e2) = fillSep [pretty e1, pretty op, pretty e2]
    pretty (RelBinary op e1 e2) = fillSep [pretty e1, pretty op, pretty e2]


instance Pretty CaseExpr where
    pretty (CaseExpr w e) = align $ vsep [prettyS "CASE",
                                          indent 4 $ vsep $ map wCl w,
                                          indent 4 $ eCl e,
                                          prettyS "END"]  

        where
            wCl (wp, we) = fillSep [prettyS "WHEN", pretty wp, prettyS "THEN", pretty we]
            eCl (Just expr) = prettyS "ELSE" <+> pretty expr
            eCl Nothing = prettyS ""

instance Pretty ArithBinOp where
    pretty Add = prettyS "+"
    pretty Subtract = prettyS "-"
    pretty Multiply = prettyS "*"
    pretty Divide = prettyS "/"
    pretty Modulo = prettyS "%"

instance Pretty RelnBinOp where
    pretty LessOrEq = prettyS "<="
    pretty GreaterOrEq = prettyS ">="
    pretty Less = prettyS "<"
    pretty Greater = prettyS ">" 
    pretty Like = prettyS "LIKE"
    pretty NotEq = prettyS "!="
    pretty Eq = prettyS "="

instance Pretty BoolBinOp where
    pretty And = prettyS "AND"
    pretty Or = prettyS "OR"

prettyS s = pretty (s :: T.Text)