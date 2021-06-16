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
    pretty (SelectStmnt selectCl fromCl whereCl groupByCl orderByCl) = 
            align $ vsep $ 
                filter nonEmpty [pSel selectCl, 
                                 pFrm fromCl, 
                                 pWhr whereCl, 
                                 pGrp groupByCl, 
                                 pOrd orderByCl]

        where
            nonEmpty :: Doc ann -> Bool
            nonEmpty doc = (length $ show doc) > 0

            -- pSel :: [(Expr, Maybe T.Text)] -> Doc ann
            pSel cols = align $ vsep [prettyS "SELECT", pCols cols]

            -- pCols :: [(Expr, Maybe T.Text)] -> Doc ann
            pCols cols = indent 4 $ encloseSep emptyDoc emptyDoc comma (pCol <$> cols)

            pCol (col, alias) = pretty col <+> pAl alias
            pAl (Just an) = prettyS "AS " <+> prettyS an 
            pAl Nothing = prettyS ""
            
            pFrm :: [FromItem] -> Doc ann
            pFrm frms = align $ prettyS "FROM " <+> vsep (pretty <$> frms)

            pWhr :: Maybe WherePredicate -> Doc ann
            pWhr (Just whr) = prettyS "WHERE " <+> (align $ pretty whr)
            pWhr Nothing = emptyDoc

            pGrp :: ColumnList -> Doc ann
            pGrp [] = emptyDoc
            pGrp cols@(x : xs) = prettyS "GROUP BY " <+> (align $ 
                                    encloseSep emptyDoc emptyDoc comma (pretty <$> cols)) 

            pOrd :: ColumnList -> Doc ann
            pOrd [] = emptyDoc
            pOrd cols@(x : xs) = prettyS "ORDER BY " <+> (align $ 
                                    encloseSep emptyDoc emptyDoc comma (pretty <$> cols))   


instance Pretty FromItem where
    pretty (FromItemJoin joinReln) = pretty joinReln
    pretty (FromItemNonJoin njReln) = pretty njReln

instance Pretty NonJoinRelation where
    pretty (TableRelation tn) = prettyS tn
    pretty (Subquery sel) = parens $ (nest 4 $ line <> pretty sel) <> line 
    pretty (FromAlias nj an) = pretty nj <+> pretty ("AS " ++ an)

instance Pretty JoinRelation where
    pretty (Join jType njReln jCrit) = pretty jType <+> pretty njReln <+> pretty jCrit

instance Pretty JoinType where
    pretty InnerJoin = prettyS "INNER JOIN" 
    pretty LeftOuterJoin = prettyS "LEFT OUTER JOIN"  
    pretty RightOuterJoin = prettyS "RIGHT OUTER JOIN"  
    pretty FullOuterJoin = prettyS "FULL OUTER JOIN" 

instance Pretty JoinCriteria where
    pretty (JoinOn expr) = line <> prettyS "ON" <+> (align $ pretty expr)  

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
    pretty And = line <> prettyS "AND"
    pretty Or = line <> prettyS "OR"

prettyS s = pretty (s :: String)

