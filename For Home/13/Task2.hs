main :: IO()
main = do
    print $ convert tree == (Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil))))

data BTree = Nil | Node Int BTree BTree
 deriving(Eq, Show)

tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

greaterOrEqualSumNumber :: Int -> BTree -> [Int]
greaterOrEqualSumNumber _ Nil = []
greaterOrEqualSumNumber n (Node value left right)
 | value >= n = [value] ++ greaterOrEqualSumNumber n left ++ greaterOrEqualSumNumber n right
 | otherwise = greaterOrEqualSumNumber n left ++ greaterOrEqualSumNumber n right

convert :: BTree -> BTree
convert Nil = Nil
convert t = helper t
 where
    helper :: BTree -> BTree
    helper Nil = Nil
    helper (Node value Nil Nil) = Node (sum $ greaterOrEqualSumNumber value t) Nil Nil
    helper (Node value left right) = Node (sum $ greaterOrEqualSumNumber value t) (helper left) (helper right)