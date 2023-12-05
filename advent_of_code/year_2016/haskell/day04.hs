{- problem statement : http://adventofcode.com/2016/day/4 -}
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe(isNothing)
import qualified Data.List as List
import qualified Data.Char as Char


type FullName = String
type Name = String
type SectorId = Int
type HashCode = String
type Sentence = String


getName :: FullName -> Name
getName =  map (\x-> if x=='-' then ' ' else x) . takeWhile (\x -> x `notElem` ['0'..'9'])

freqTable :: Name -> [(Char, Int)]
freqTable name = computeFreqTable HashMap.empty name
    where
        computeFreqTable hashmap (x:xs)
            | isNothing oldValue = computeFreqTable (HashMap.insert x 1 hashmap) xs
            | otherwise = computeFreqTable (HashMap.adjust (+1) x hashmap) xs
            where oldValue = HashMap.lookup x hashmap;
        computeFreqTable hashmap [] = HashMap.toList hashmap

getSectorId :: Name -> SectorId
getSectorId = read . takeWhile isNumber . dropWhile (not.isNumber)

isNumber :: Char -> Bool
isNumber = flip elem ['0'..'9']
isLetter :: Char -> Bool
isLetter = flip elem ['a'..'z']
{- compute the hash code from the data given
 - in http://adventofcode.com/2016/day/4 -}
getCode :: Name -> HashCode
getCode = take 5 . map fst . List.sortBy cmpFreq . freqTable . getEncryptedName

{- get the hash at the end of the string made of only 5
 - letters enclosed into a pair of brackets ([])-}
getHash :: FullName -> HashCode
getHash = take 5 . tail . dropWhile ('[' /=)

cmpFreq :: (Char, Int) -> (Char, Int) -> Ordering
cmpFreq (x,y) (x',y') = compare (y',x) (y,x')

getEncryptedName :: FullName -> Name
getEncryptedName = filter isLetter . takeWhile (not.isNumber)

isValidName :: FullName -> Bool
isValidName fullname = getHash fullname == getCode fullname

translateSentence :: FullName -> String
translateSentence = \x -> translateSentence' (getSectorId x) x

translateSentence' :: Int -> Name -> Sentence
translateSentence' n = map (\x -> if x `elem` ['a'..'z'] then shiftN n x else x)

shiftN :: Int -> Char -> Char
shiftN n x = Char.chr ( (Char.ord x - 97 + n) `mod` 26 + 97 )

ex4' = [ "aaaaa-bbb-z-y-x-123[abxyz]"
        ,"a-b-c-d-e-f-g-h-987[abcde]"
        ,"not-a-real-room-404[oarel]"
        ,"totally-real-room-200[decoy]" ]

ex4 = lines <$> readFile "input_day_4.txt"
answer = sum . map getSectorId . filter isValidName <$> ex4
answer' = map translateSentence $ filter isValidName ex4'
answer2 = map translateSentence . filter isValidName <$> ex4
lastAnswer = filter (List.isInfixOf "north") <$> answer2
main :: IO ()
main = do
    x <- lastAnswer
    putStrLn(show x)
    return ()
