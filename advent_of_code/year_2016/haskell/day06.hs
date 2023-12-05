import Data.Map.Strict as Map

import qualified Data.List as List
import qualified Data.Map as Map

getData :: IO [String]
getData = lines <$> readFile "input_day_6.txt"

freqTable :: [String] -> [Map.Map Char Int] -> [Map.Map Char Int]
freqTable (x:xs) inputCounter = freqTable xs (countOccurences x inputCounter)
freqTable [] inputCounter = inputCounter

countOccurences :: String -> [Map.Map Char Int] -> [Map.Map Char Int]
countOccurences charList theMap = zipWith countOcc charList theMap

countOcc :: Char -> Map.Map Char Int -> Map.Map Char Int
countOcc x theMap =  insertWith (\new old -> old+1) x 1 theMap

startCount :: [Map.Map Char Int]
startCount = take 8 (repeat Map.empty)

result :: IO [Map.Map Char Int]
result = (flip freqTable) startCount <$> getData

password :: IO String
password = List.map ( fst . head . (List.sortBy getFreq) . toList ) <$> result
password2 = List.map ( fst . head . (List.sortBy invFreq) . toList ) <$> result

getFreq :: (Char, Int) -> (Char, Int) -> Ordering
getFreq (a, y) (a', y') = compare y' y
invFreq :: (Char, Int) -> (Char, Int) -> Ordering
invFreq (a, y) (a', y') = compare y y'
