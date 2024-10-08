import Data.List

main :: IO()
main = do
    print $ toBinaryIndexed tree == Node (10,5) (Node (5,2) (Node (3,1) (Node (1,0) Nil Nil) Nil) (Node (7,4) (Node (6,3) Nil Nil) Nil)) (Node (15,7) (Node (13,6) Nil Nil) (Node (18,8) Nil Nil))

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

tree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

traverseDFS :: BTree a -> [a]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

getIndex :: (Eq a) => [a] -> a -> Int -> Int
getIndex [] _ _ = error "Element is not in the list!"
getIndex (x:xs) element index
 | element == x = index
 | otherwise = getIndex xs element (index + 1)

toBinaryIndexed :: (Eq a) => BTree a -> BTree (a,Int)
toBinaryIndexed Nil = Nil
toBinaryIndexed t = helper t (traverseDFS t)
 where
    helper :: (Eq a) => BTree a -> [a] -> BTree (a,Int)
    helper Nil _ = Nil
    helper (Node value left right) ts = Node (value, getIndex ts value 0) (helper left ts) (helper right ts)