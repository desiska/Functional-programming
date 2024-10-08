main :: IO()
main = do
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2

getOddCompositionValue :: [Int -> Int] -> Int -> Int
getOddCompositionValue fs x = let listOdd = [f  | (f, index) <- zip fs [1..], odd index] in foldl(\ acc f -> f acc ) x listOdd