main :: IO ()
main = putStrLn "Test suite not yet implemented"


-- tparseEof fromClause "from A join B on A.col = B.col left outer join C on B.col = C.col join D on B.col=D.col"


-- Right [
--     FromItemNonJoin (TableRelation "A"),
--     FromItemJoin (Join (FromItemNonJoin (TableRelation "A")) InnerJoin (TableRelation "B") (JoinOn (RelBinary Eq (QfdIdentifier "A" "col") (QfdIdentifier "B" "col")))),
--     FromItemJoin (Join (FromItemNonJoin (TableRelation "A")) LeftOuterJoin (TableRelation "C") (JoinOn (RelBinary Eq (QfdIdentifier "B" "col") (QfdIdentifier "C" "col")))),
--     FromItemJoin (Join (FromItemNonJoin (TableRelation "A")) InnerJoin (TableRelation "D") (JoinOn (RelBinary Eq (QfdIdentifier "B" "col") (QfdIdentifier "D" "col"))))]


-- Right [
    -- FromItemNonJoin (TableRelation "A"),
    -- FromItemJoin (Join InnerJoin (TableRelation "B") (JoinOn (RelBinary Eq (QfdIdentifier "A" "col") (QfdIdentifier "B" "col")))),
    -- FromItemJoin (Join LeftOuterJoin (TableRelation "C") (JoinOn (RelBinary Eq (QfdIdentifier "B" "col") (QfdIdentifier "C" "col")))),
    -- FromItemJoin (Join InnerJoin (TableRelation "D") (JoinOn (RelBinary Eq (QfdIdentifier "B" "col") (QfdIdentifier "D" "col"))))]

