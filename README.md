
## hs-sql-parser: A parser written in Haskell for simple SQL like queries


        Usage: hs-sql-parser-exe [-i|--input-file INPUTFILE]
                                [-o|--output-file OUTPUTFILE] [-p|--pretty]
        Parse the SQL query and pretty print the AST

        Available options:
        -i,--input-file INPUTFILE
                                Input File
        -o,--output-file OUTPUTFILE
                                Output File
        -p,--pretty              Whether to pretty print the AST
        -h,--help                Show this help text


Eg: 

        > cat test/resources/t.sql

                select a.c1, a.c2
                from tab1 as a
                inner join tab2 as b
                on a.c11 = b.c11 and a.c12 = b.c12
                left join (select * from tab3 as x -- where c44 = 99 group by c35 order by c34
                inner join (select * from tab31 where c1 = 45) as y
                on x.c1 = y.c1 and x.c2 = y.c2
                ) as e
                on b.c22 = e.c22
                where c3 = 44 and c4 = 'open' and c5 = 66

        > stack run -- -i test/resources/t.sql -p
                SELECT
                    a.c1 ,a.c2
                FROM  tab1 AS a
                INNER JOIN tab2 AS b
                ON a.c11 = b.c11
                   AND a.c12 = b.c12
                LEFT OUTER JOIN (
                    SELECT
                        *
                    FROM  tab3 AS x
                    INNER JOIN (
                        SELECT
                            *
                        FROM  tab31
                        WHERE  c1 = 45
                    ) AS y
                    ON x.c1 = y.c1
                       AND x.c2 = y.c2
                ) AS e
                ON b.c22 = e.c22
                WHERE  c3 = 44
                       AND c4 = 'open'
                       AND c5 = 66

        > stack run -- -i test/resources/t.sql

        SelectStmnt {selectCl = [(QfdIdentifier "a" "c1",Nothing),(QfdIdentifier "a" "c2",Nothing)], fromCl = [FromItemNonJoin (FromAlias (TableRelation "tab1") "a"),FromItemJoin (Join InnerJoin (FromAlias (TableRelation "tab2") "b") (JoinOn (BoolBinary And (RelBinary Eq (QfdIdentifier "a" "c11") (QfdIdentifier "b" "c11")) (RelBinary Eq (QfdIdentifier "a" "c12") (QfdIdentifier "b" "c12"))))),FromItemJoin (Join LeftOuterJoin (FromAlias (Subquery (SelectStmnt {selectCl = [(Asterisk,Nothing)], fromCl = [FromItemNonJoin (FromAlias (TableRelation "tab3") "x"),FromItemJoin (Join InnerJoin (FromAlias (Subquery (SelectStmnt {selectCl = [(Asterisk,Nothing)], fromCl = [FromItemNonJoin (TableRelation "tab31")], whereCl = Just (RelBinary Eq (Identifier "c1") (IntegralLit 45)), groupByCl = [], orderByCl = []})) "y") (JoinOn (BoolBinary And (RelBinary Eq (QfdIdentifier "x" "c1") (QfdIdentifier "y" "c1")) (RelBinary Eq (QfdIdentifier "x" "c2") (QfdIdentifier "y" "c2")))))], whereCl = Nothing, groupByCl = [], orderByCl = []})) "e") (JoinOn (RelBinary Eq (QfdIdentifier "b" "c22") (QfdIdentifier "e" "c22"))))], whereCl = Just (BoolBinary And (BoolBinary And (RelBinary Eq (Identifier "c3") (IntegralLit 44)) (RelBinary Eq (Identifier "c4") (StringLit "open"))) (RelBinary Eq (Identifier "c5") (IntegralLit 66))), groupByCl = [], orderByCl = []}%


