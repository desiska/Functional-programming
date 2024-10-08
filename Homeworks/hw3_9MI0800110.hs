import Data.List
import Data.Char

main :: IO()
main = do
    print $ "Task1:"
    print $ findMaxPalindrome 1112332 == 3211123
    print $ findMaxPalindrome 22220 == 22022
    print $ findMaxPalindrome 2205 == 252
    print $ findMaxPalindrome 120021 == 210012
    print $ findMaxPalindrome 12320 == 232
    print $ findMaxPalindrome 123 == 3
    
    print $ "Task2:"
    print $ calculate "1+2+x" [('x', 5)] == 8
    print $ calculate "x+2+x-2+y+z" [('x', 1), ('y', 2), ('z', 3)] == 7
    print $ calculate "x+2-x-2+y+x" [('x', 1), ('y', -15)] == -14
    print $ calculate "y+2+x-2+z+z+z+x+5" [('x', 1), ('y', 2), ('z', 3)] == 18
    print $ calculate "8-2" [] == 6
    print $ calculate "5" [] == 5

--Task 1
{-grouping :: Integer -> [String]
grouping = group . sort . show

makePairDescendingOrdered :: [String] -> [(Int, Int)]
makePairDescendingOrdered = reverse . map (\ x -> (read $ nub x, length x))

replicateDigit :: Int -> Int -> String
replicateDigit digit counter = concat $ replicate counter $ show digit

makePalindrome :: [(Int, Int)] -> Integer
makePalindrome = helper (-1) ""
 where
    helper :: Int -> String -> [(Int, Int)] -> Integer
    helper (-1) result [] = read (result ++ reverse result)
    helper centre result [] = read (result ++ (show centre) ++ reverse result)
    helper centre result ((n, len):ys)
     | mod len 2 == 0 = helper centre (result ++ replicateDigit n (div len 2)) ys
     | otherwise = helper (max centre n) (result ++ replicateDigit n (div len 2)) ys

findMaxPalindrome :: Integer -> Integer
findMaxPalindrome = makePalindrome . makePairDescendingOrdered . grouping-}

createList :: Integer -> [Integer]
createList x
 | x < 0 = error "Imput must be a non-negative integer"
 | x == 0 = []
 | otherwise = rem x 10 : createList(div x 10)

backToNum :: [Integer] -> Integer
backToNum = foldl (\acc x -> x + acc * 10) 0

isPalindrome :: Integer -> Bool
isPalindrome n = (show n) == (reverse $ show n)

findMaxPalindrome :: Integer -> Integer
findMaxPalindrome n = maximum $ filter isPalindrome (map backToNum (concat $ map permutations (subsequences $ createList n)))


--Task 2
representation :: Char -> [(Char, Int)] -> Int
representation _ [] = error "Invalid input!"
representation symbol ((var, value):ys)
 | symbol == var = value
 | otherwise = representation symbol ys

calculation :: Int -> String -> [(Char, Int)] -> Int
calculation result "" _ = result
calculation result (s:n:ns) xs
 | s == '+' && isDigit n = calculation (result + (digitToInt n)) ns xs
 | s == '-' && isDigit n = calculation (result - (digitToInt n)) ns xs
 | s == '+' = calculation (result + (representation n xs)) ns xs
 | s == '-' = calculation (result - (representation n xs)) ns xs
 | otherwise = error "Invalid input!"


calculate :: String -> [(Char, Int)] -> Int
calculate (s:"") _ = digitToInt s
calculate (s:ss) xs
 | isDigit s = calculation (digitToInt s) ss xs
 | otherwise = calculation (representation s xs) ss xs