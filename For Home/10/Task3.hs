main :: IO()
main = do
    print $ removeAll 5 [5] == []
    print $ removeAll 4 [4, 4] == []
    print $ removeAll 5 [1] == [1]
    print $ removeAll 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAll 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]
    
    -- same for removeAllHOF
    print $ removeAllHOF 5 [5] == []
    print $ removeAllHOF 4 [4, 4] == []
    print $ removeAllHOF 5 [1] == [1]
    print $ removeAllHOF 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAllHOF 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

removeAll :: (Eq a) => a -> [a] -> [a]
removeAll _ [] = []
removeAll n (x:xs)
 | x == n = removeAll n xs
 | otherwise = x : removeAll n xs


removeAllHOF :: (Eq a) => a -> [a] -> [a]
removeAllHOF n = filter (/= n)