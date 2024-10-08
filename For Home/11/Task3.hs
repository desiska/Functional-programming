import Data.List

main :: IO()
main = do
    print $ (numAdvance 5) [10, 9, 8, 7, 7, 7, 5, 5] == 6
    print $ (numAdvance 2) [0, 0, 0, 0] == 0
    print $ (numAdvance 3) [10, 9, 8, 7, 7, 7, 5, 5] == 3
    print $ (numAdvance 1) [10, 9, 8, 7, 7, 7, 5, 5] == 1
    print $ (numAdvance 2) [10, 9, 8, 7, 7, 7, 5, 5] == 2
    print $ (numAdvance 9) [5, 5, 5, 3, 3, 3, 0, 0, 0, 0] == 6
    print $ (numAdvance 10) [5, 5, 5, 3, 3, 3, 0, 0, 0, 0] == 6

numAdvance :: (Ord a, Num a) => Int -> ([a] -> Int)
numAdvance k = (\xs -> length $ filter (\x -> x >= xs !! (k - 1) && x > 0) xs)