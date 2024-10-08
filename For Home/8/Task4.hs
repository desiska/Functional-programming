main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

areAmicable :: Integer -> Integer -> Bool
areAmicable x1 x2 = sum [x | x <- [1..x1 `div` 2], mod x1 x == 0] == x2 && sum [x | x <- [1..x2 `div` 2], mod x2 x == 0] == x1