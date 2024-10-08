import Data.Char

main :: IO()
main = do
    print $ controller "" == ""
    print $ controller ".........." == "0000000000"
    print $ controller "P...." == "12345"
    print $ controller "P.P.." == "12222"
    print $ controller "..P...O..." == "0012343210"
    print $ controller "P......P......" == "12345554321000"
    print $ controller "P.P.P...." == "122234555"
    print $ controller ".....P.P........P...." == "000001222222222234555"
    print $ controller ".........." == "0000000000"
    print $ controller "P.." == "123"
    print $ controller "P...." == "12345"
    print $ controller "P......P......" == "12345554321000"
    print $ controller "P.P.." == "12222"
    print $ controller "P.P.P...." == "122234555"
    print $ controller ".....P.P........P...." == "000001222222222234555"
    print $ controller ".....P......P.P..P...." == "0000012345554333321000"
    print $ controller "P.O...." == "1210000"
    print $ controller "P......P.O...." == "12345554345555"
    print $ controller "P..OP..P.." == "1232222100"
    print $ controller "P......P..OP..P..." == "123455543233334555"
    print $ controller "..P...O....." == "001234321000"


commands :: String -> Bool -> Bool -> Int -> String -> String
commands [] _ _ _ result = reverse result
commands ('P':xs) False True position result = commands ('.':xs) True True (position + 1) result
commands ('P':xs) False False position result = commands ('.':xs) True False (position - 1) result
commands ('P':xs) True True position result = commands xs False True (position - 1) ((intToDigit (position - 1)):result)
commands ('P':xs) True False position result = commands xs False False (position + 1) ((intToDigit (position + 1)):result)
commands ('O':xs) True True position result = commands ('.':xs) True False (position - 2) result
commands ('O':xs) True False position result = commands ('.':xs) True True (position + 2) result
commands (x:xs) True True 5 result = commands xs False False 5 ('5':result)
commands (x:xs) True False 0 result = commands xs False True 0 ('0':result)
commands (x:xs) True True position result = commands xs True True (position + 1) ((intToDigit position):result)
commands (x:xs) True False position result = commands xs True False (position - 1) ((intToDigit position):result)
commands (x:xs) False isOpenDirection position result = commands xs False isOpenDirection position ((intToDigit position):result)

controller :: String -> String
controller str = commands str False True 0 []