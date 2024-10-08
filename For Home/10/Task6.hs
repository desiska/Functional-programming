import Data.List

main :: IO()
main = do
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 5] == (True, 1)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 7] == (True, 3)
    print $ isImage [4, 5, 6, 7] [1, 2, 3, 4] == (True, -3)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 6] == (False, 0)
    print $ isImage [1, 2] [-1, -2] == (False, 0)
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 4] == (False, 0)

isImage :: (Num a, Eq a) => [a] -> [a] -> (Bool, a)
isImage [] [] = (True, 0)
isImage (x:xs) (y:ys) = helper (length (nub $ zipWith (-) (x:xs) (y:ys)) == 1, y - x)
 where
    helper :: (Num a) => (Bool, a) -> (Bool, a)
    helper (False, _) = (False, 0)
    helper (x, y) = (x, y)