import Data.List
import Data.Char

main :: IO ()
main = do
    --Task 1
    sample <- readFile "Homeworks\\sample.txt"
    input1 <- readFile "Homeworks\\input1.txt"
    input2 <- readFile "Homeworks\\input2.txt"
    --print (lines commands)
    print $ "Task 1:"
    print $ ((getDirSize sample) ["/", "a", "e"]) == 584
    print $ ((getDirSize sample) ["/", "d"]) == 24933642
    print $ ((getDirSize sample) ["/"]) == 48381165
    --print ((getDirSize sample) ["/", "a", "f"]) --error "not found"
    print $ ((getDirSize commands) ["/"]) == 7
    print $ ((getDirSize commands) ["/", "a"]) == 7
    print $ ((getDirSize commands) ["/", "a", "a"]) == 3
    print $ ((getDirSize commands) ["/", "a", "b"]) == 4

    --Task 2
    print $ "Task 2:"
    print $ needToFreeUp sample 9380714 -- == ("e",584)
    print $ needToFreeUp sample 49999843 -- == ("/",48381165)
    print $ needToFreeUp sample 30000000 -- == ("d",24933642)
    print $ needToFreeUp input1 2676125 -- == ("lmw",8097)
    print $ needToFreeUp input1 54535473 -- == ("/",40389918)
    print $ needToFreeUp input1 48560142 -- == ("zpdnprb",20646114)
    print $ needToFreeUp input1 37961783 -- == ("jvwtm",8509877)
    print $ needToFreeUp input1 34535473 -- == ("twpq",5296583)
    print $ needToFreeUp input1 30000000 -- == ("mdclfbs",404395)
    print $ needToFreeUp input2 2676125 -- == ("tpcwhmv",3666)
    print $ needToFreeUp input2 54535473 -- == ("/",40913445)
    print $ needToFreeUp input2 48560142 -- == ("/",40913445)
    print $ needToFreeUp input2 37961783 -- == ("jssbn",13136144)
    print $ needToFreeUp input2 34535473 -- == ("pjzpjjq",6986416)
    print $ needToFreeUp input2 30000000 -- == ("lfrctthp",942298)
 where
    commands = "$ cd /\n$ ls\ndir a\n$ cd a\n$ ls\ndir a\ndir b\n$ cd a\n$ ls\n1 f.txt\n2 g.txt\n$ cd ..\n$ cd b\n$ ls\n4 h.txt"

type Commands = String
type Size = Int
type Name = String

--Task 1

getSize :: Commands -> Size -> Size
getSize [] _ = 0
getSize (' ':cs) result = result
getSize (c:cs) result = getSize cs (result * 10 + digitToInt c)

sizeOfDirectory :: [Commands] -> Commands -> [Name] -> Size
sizeOfDirectory [] _ _ = 0
sizeOfDirectory (c:cs) cmd dirs
 | isPrefixOf "$ cd " c = 0
 | isPrefixOf "dir " c = (getDirSize cmd) (dirs ++ [(drop 4 c)]) + sizeOfDirectory cs cmd dirs
 | "$ ls" == c = sizeOfDirectory cs cmd dirs
 | otherwise = getSize c 0 + sizeOfDirectory cs cmd dirs

getDirSize :: Commands -> ([Name] -> Size)
getDirSize cmd = (\ dirs -> helper dirs (lines cmd) dirs)
 where
  helper :: [Name] -> [Commands] -> [Name] -> Size
  helper [] [] _ = 0
  helper _ [] _ = error "not found"
  helper (d:[]) cs dirs = sizeOfDirectory (drop ((head $ elemIndices ("$ cd " ++ d) cs) + 1) cs) cmd dirs
  helper (d:ds) (c:cs) dirs
   | ("$ cd " ++ d) == c = helper ds cs dirs
   | ("dir " ++ d) == c = helper (d:ds) cs dirs
   | otherwise = helper (d:ds) cs dirs



--Task 2

allDirectorySize :: [Commands] -> [Name] -> Commands -> [(Name, Size)] -> [(Name, Size)]
allDirectorySize [] _ _ result = result
allDirectorySize (c:cd) address cmd result
 | "$ cd .." == c =
    let newAddress = take (length address - 1) address
    in allDirectorySize cd newAddress cmd result
 | isPrefixOf "$ cd " c =
    let newAddress = address ++ [drop 5 c]
        newSize = getDirSize cmd newAddress
    in allDirectorySize cd newAddress cmd ((drop 5 c, newSize):result)
 | otherwise =
    allDirectorySize cd address cmd result

needToFreeUp :: Commands -> Size -> (Name, Size)
needToFreeUp cmd needed = head $ sortOn snd $ filter (\ (name,size) -> size >= needed - (70000000 - (getDirSize cmd) ["/"])) $ allDirectorySize (lines cmd) [] cmd []