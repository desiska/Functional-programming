main :: IO()
main = do
    print $ primesInRange 1 100 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
    print $ primesInRange 100 1 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

isPrime :: Int -> Bool
isPrime n = n > 1 && sum [x | x <- [2 .. div n 2], mod n x == 0] == 0

primesInRange :: Int -> Int -> [Int]
primesInRange n1 n2 = [x | x <- [min n1 n2 .. max n1 n2], isPrime x]