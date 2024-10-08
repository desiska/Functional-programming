main :: IO()
main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False

hasIncDigits :: Integer -> Bool
hasIncDigits n = helper (div n 10) (mod n 10)
 where
    helper :: Integer -> Integer -> Bool
    helper leftover prev
        | leftover == 0 = True
        | (mod leftover 10) > prev = False
        | otherwise = helper (div leftover 10) (mod leftover 10)
