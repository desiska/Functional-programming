main :: IO()
main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6


reverseOrdSuff :: Int -> Int
reverseOrdSuff = helper 0
 where
    helper :: Int -> Int -> Int
    helper result 0 = result
    helper result leftover
     | mod leftover 10 > mod result 10 = helper (result * 10 + mod leftover 10) (div leftover 10)
     | otherwise = result