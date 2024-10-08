import Data.List
import Data.Char

main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD

reduceStr :: String -> String
reduceStr (x:xs) = reverse $ helper [x] xs
 where
    helper :: String -> String -> String
    helper result [] = result
    helper result [x] = (x:result)
    helper (r:rs) (y1:y2:ys)
     | isDuplicate r y1 = helper rs (y2:ys)
     | isDuplicate y1 y2 = helper (r:rs) ys
     | otherwise = helper (y1:r:rs) (y2:ys)

isDuplicate :: Char -> Char -> Bool
isDuplicate c1 c2 = (isLower c1 && toUpper c1 == c2) || (isUpper c1 && toLower c1 == c2)
   