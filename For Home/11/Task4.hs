import Data.Char

main :: IO()
main = do
    print $ squareDigits 9119 == 811181
    print $ squareDigits (-9119) == -811181

squareDigits :: Int -> Int
squareDigits n = if signum n >= 0 then read $ concatMap (show . (^2) . digitToInt) (show n) else (-1) * (read $ concatMap (show . (^2) . digitToInt) (tail $ show n))