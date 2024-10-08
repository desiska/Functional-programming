main :: IO()
main = do
    print $ isPrimeG 1 == False
    print $ isPrimeG 2 == True
    print $ isPrimeG 3 == True
    print $ isPrimeG 6 == False
    print $ isPrimeG 61 == True

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True

isPrimeG :: Integer -> Bool
isPrimeG n = helper n 2
 where
    helper :: Integer -> Integer -> Bool
    helper n leftover
        | leftover > sqrt n = True
        | mod n leftover == 0 = False
        | otherwise = helper n (leftover + 1)

isPrimeLC :: Integer -> Bool
isPrimeLC n = n > 1 && sum [x | x <- [2..floor (sqrt (fromIntegral n))], mod n x == 0] == 0