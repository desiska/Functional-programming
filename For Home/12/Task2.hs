main :: IO()
main = do
    print $ colourBTree
    print $ highest Red colourBTree == 4
    print $ highest Green colourBTree == 3
    print $ highest Blue colourBTree == 4

data Colour = Red | Green | Blue
 deriving (Show, Eq)

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show)

colourBTree = Node Blue (Node Green (Node Blue (Node Red Nil Nil) Nil) (Node Blue Nil Nil)) (Node Red (Node Green (Node Blue Nil Nil) Nil) (Node Red Nil Nil))

highest :: Colour -> BTree Colour -> Int
highest _ Nil = 0;
highest colour tree = maximum $ helper 1 tree
 where
    helper :: Int -> BTree Colour -> [Int]
    helper _ Nil = []
    helper level (Node value left right)
     | value == colour = [level] ++ helper (level + 1) left ++ helper (level + 1) right
     | otherwise = helper (level + 1) left ++ helper (level + 1) right 