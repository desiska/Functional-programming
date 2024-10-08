import Data.List

main :: IO()
main = do
    print $ leavesAreEqual t1 t2 == True
    print $ leavesAreEqual t3 t4 == False

data BTree = Nil | Node Int BTree BTree

t1 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))
t2 = Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil))
t3 = t1
t4 =  Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)))

getLeaves :: BTree -> [Int]
getLeaves Nil = []
getLeaves (Node value Nil Nil) = [value]
getLeaves (Node value left right) = getLeaves left ++ getLeaves right

leavesAreEqual :: BTree -> BTree -> Bool
leavesAreEqual _ Nil = False
leavesAreEqual Nil _ = False
leavesAreEqual Nil Nil = True
leavesAreEqual (Node value1 Nil Nil) (Node value2 Nil Nil) = value1 == value2
leavesAreEqual bt1 bt2 = (sort $ getLeaves bt1) == (sort $ getLeaves bt2)