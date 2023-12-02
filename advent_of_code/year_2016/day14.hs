import Data.String
import qualified Data.List as List
import qualified Data.Map.Strict as HashMap
import qualified Data.Hash.MD5 as MD5

puzzleInput :: String
puzzleInput = "ihaygndm"
testInput = "abc"


ok3hashes = map (hasIdenticalDigits 3) hashList
ok5hashes = map (hasIdenticalDigits 5) hashList

hasIdenticalDigits :: Int -> String -> Maybe Char
hasIdenticalDigits n (x:xs)
    | length identical == (n-1) = x
    | otherwise = hasIdenticalDigits n remaining
    where identical = takeWhile (==x) xs
          remaining = dropWhile (==x) xs
hasIdenticalDigits n [] = Nothing


okInteger :: Int -> Bool
okInteger x
    | digits == [] = False
    | otherwise = any (isKey x) digits
    where digits = hasThreeIdenticalDigits (getHash x)

nextThousandHashes :: Int -> [String]
nextThousandHashes x = map hasFiveIdenticalDigits (map getHash [(x+1)..(x+1000)])

isKey :: Int -> Char -> Bool
isKey x letter = any (letter `elem`) (nextThousandHashes x)

nDigitSet :: Int -> [(Int, String)] -> HashMap.Map Int String
nDigitSet n = HashMap.fromList (filter ((\(x,y)-> y/="")))

main :: IO()
main = do
    fileContent <- readFile "output_ihaygndm_200000_hashes.txt"
    inputList <- map ((\[x,y]-> (read x::Int, y)).words) (lines . fileContent)
    mapM_ putStrLn inputList
    return ()
