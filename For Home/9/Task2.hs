import Data.Char

main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

isPrime :: Int -> Bool
isPrime n = n > 1 && sum [x | x <- [2 .. floor $ sqrt $ fromIntegral n], mod n x == 0] == 0

isContainDigit :: Int -> Int -> Bool
isContainDigit n d = elem (intToDigit d) $ show n

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n [x | x <- [2 ..], isPrime x && isContainDigit x d]