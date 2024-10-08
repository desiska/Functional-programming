import Data.List

main :: IO()
main = do
    -- print $ uniqueNumber [1,2,3,2]
    print $ sumUnique [[1,2,3,2],[1,-4],[1]] -- == 2
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45

-- uniqueNumber :: [Int] -> [Int]
uniqueNumber xs = [head gr | gr <- group $ sort xs, length gr == 1]


-- (xs:xss) = helper (group $ sort xs) ++ uniqueNumber xss
--  where
--     helper :: [[Int]] -> [Int]
--     helper [] = []
--     helper (x:xs)
--      | length x == 1 = x ++ helper xs
--      | otherwise = helper xs


-- sumUnique :: [[Int]] -> Int
sumUnique = sum . map (sum . uniqueNumber)