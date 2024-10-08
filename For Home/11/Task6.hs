main :: IO()
main = do
    print $ matching "1234" == []
    print $ matching ",[.[-],]" == [(3,5),(1,7)]
    print $ matching ",+[-.,+]" == [(2,7)]
    print $ matching "[][]" == [(0,1),(2,3)]

matching :: String -> [(Int, Int)]
matching str = reverse $ helper str [] 0 []
  where
    helper [] _ _ result = result
    helper ('[':xs) stack index result = helper xs (index:stack) (index + 1) result
    helper (']':xs) (s:sx) index result = helper xs sx (index + 1) ((s, index):result)
    helper (x:xs) stack index result = helper xs stack (index + 1) result