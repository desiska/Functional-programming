main :: IO()
main = do
    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5

sqAvg :: Integer -> Integer -> Double
sqAvg x1 x2 = fromIntegral (x1 * x1 + x2 * x2) / 2.0