main :: IO()
main = do
    print $ stocklist stocks ['A','B'] == [('A',200),('B',1140)]
    print $ stocklist stocks ['C','X'] == [('C',500),('X',0)]
    print $ stocklist stocks ['Y','X'] == [('Y',0),('X',0)]
    print $ stocklist stocks ['C'] == [('C', 500)]

data Stock = Stock String Int
stocks = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]

sumStock :: [Stock] -> Char -> Int
sumStock stocks category = sum [quantity | Stock (c:cs) quantity <- stocks, c == category]

stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist [] [] = []
stocklist stocks [] = []
stocklist [] categories = []
stocklist stocks categories = map (\ category -> (category, sumStock stocks category)) categories