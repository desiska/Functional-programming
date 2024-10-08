import Data.List

main :: IO()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ cone numberBTree == True

data BTree = Nil | Node Int BTree BTree

numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

getLevel :: BTree -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) level = getLevel left (level - 1) ++ getLevel right (level - 1)

levelSum :: BTree -> Int -> Int
levelSum tree level = sum $ getLevel tree level

height :: BTree -> Int 
height Nil = 0
height (Node _ left right) = max ( 1 + height left) (1 + height right)

getListOfLevels :: BTree -> Int -> Int ->  [[Int]]
getListOfLevels tree level max = if level <= max then [getLevel tree level] ++ getListOfLevels tree (level + 1) max else []

cone :: BTree -> Bool
cone Nil = False
cone (Node _ Nil Nil) = False
cone tree = helper tree == (sort $ helper tree)
 where
    helper :: BTree -> [Int]
    helper Nil = []
    helper tree = map sum (take ((length $ getListOfLevels tree 0 (height tree)) - 1) (getListOfLevels tree 0 (height tree)))