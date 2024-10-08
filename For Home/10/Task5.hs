main :: IO()
main = do
    print $ combine [(1, 2), (1, 2)] == (11, 22)
    print $ combine [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] == (377802, 989859)

combine :: (Ord a, Num a) => [(a, a)] -> (a, a)
combine [] = error "Input list must contain at least one element."
combine [(x, y)] = (x, y)
combine ((r1, r2):(x1, x2):xs) = combine (((min r1 r2) * 10 + min x1 x2, (max r1 r2) * 10 + max x1 x2) : xs)
