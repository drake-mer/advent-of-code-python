{-
 - source : http://adventofcode.com/2016/day/2
 -
 - Day 2 of advent of code. BathRoom password
 -}
import qualified Data.Maybe as Mb
type Move = Char
type Key = Int
ex1 = ["ULL","RRDDD","LURDL","UUUUD"]
ex2 :: IO [String]
ex2 = do
  content <- readFile "input_day_2.txt"
  return (lines content)
dir = ['U','L','R','D']
k1 = [1,1,2,4]
k2 = [2,1,3,5]
k3 = [3,2,3,6]
k4 = [1,4,5,7]
k5 = [2,4,6,8]
k6 = [3,5,6,9]
k7 = [4,7,8,7]
k8 = [5,7,9,8]
k9 = [6,8,9,9]

keyMap = zip [1..] [k1, k2, k3, k4, k5, k6, k7, k8, k9]

k1' = [1,1,1,3]
k2' = [2,2,3,6]
k3' = [1,2,4,7]
k4' = [4,3,4,8]
k5' = [5,5,6,5]
k6' = [2,5,7,10]
k7' = [3,6,8,11]
k8' = [4,7,9,12]
k9' = [9,8,9,9]
kA' = [6,10,11,10]
kB' = [7,10,12,13]
kC' = [8,11,12,12]
kD' = [12,13,13,13]
keyMap' = zip [1..] [k1', k2', k3', k4', k5', k6', k7', k8', k9', kA', kB', kC', kD']

mapInstructionToKeys :: Key -> [Move] -> [Int]
mapInstructionToKeys initKey instructions = mapInstructionsToKeys' initKey instructions []


mapInstructionsToKeys' :: Key -> [Move] -> [Int] -> [Int]
mapInstructionsToKeys' initKey (x:xs) moves = mapInstructionsToKeys' newKey xs (newKey:moves)
  where newKey = Mb.fromJust $ getNextKey x initKey;getNextKey=selectedGetNextKey

mapInstructionsToKeys' _ [] moves = reverse moves


selectedGetNextKey = getNextKeyFancyKeyPad

getNextKeyStandardKeyPad :: Move -> Key -> Maybe Key
getNextKeyStandardKeyPad move key = lookup move =<< zip dir <$> (lookup key keyMap)

getNextKeyFancyKeyPad :: Move -> Key -> Maybe Key
getNextKeyFancyKeyPad move key = lookup move =<< zip dir <$> (lookup key keyMap')


getCode :: [Move] -> Key -> Key
getCode [] x = x
getCode (x:xs) key = getCode xs (Mb.fromMaybe key (getNextKey x key))
  where getNextKey=selectedGetNextKey

getFullCode :: [[Move]] -> [Key]
getFullCode = getFullCode' 5 []


getFullCode' :: Key -> [Key] -> [[Move]] -> [Key]
getFullCode' key stored (moveList:nextMoves) = newKey:(getFullCode' newKey (newKey:stored) nextMoves)
  where newKey = getCode moveList key
getFullCode' _ _ [] = []
