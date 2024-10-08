import Data.List

main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False

data BTree a = Nil | Node a (BTree a) (BTree a)

t1 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (4,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))
t2 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (7,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))

intervalTree :: (Num a) => BTree (a,a) -> BTree a
intervalTree tree = helper (\ (x,y) -> y - x) tree
 where
    helper :: (a -> b) -> BTree a -> BTree b
    helper _ Nil = Nil
    helper f (Node value left right) = Node (f value) (helper f left) (helper f right)

isOrdered :: (Ord a, Eq a) => BTree a -> Bool
isOrdered tree = helper tree == (sort $ helper tree)
 where
    helper :: BTree a -> [a]
    helper Nil = []
    helper (Node value left right) = helper left ++ [value] ++ helper right

ordered :: (Num a, Ord a) => BTree (a,a) -> Bool
ordered = isOrdered . intervalTree