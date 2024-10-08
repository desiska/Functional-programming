main :: IO()
main = do
    print $ myGcdG 5 13 == 1
    print $ myGcdG 13 1235 == 13

    print $ myGcdPM 5 13 == 1
    print $ myGcdPM 13 1235 == 13

myGcdG :: Integer -> Integer -> Integer
myGcdG x y
 | x == 0 = y
 | y == 0 = x
 | otherwise = myGcdG (min x y) (mod (max x y) (min x y))

myGcdPM :: Integer -> Integer -> Integer
myGcdPM 0 y = y
myGcdPM x 0 = x
myGcdPM x y = myGcdPM (min x y) (mod (max x y) (min x y))