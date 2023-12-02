import qualified Data.List.Split as Split

test_part_1 = [  "abba[mnop]qrst"
                , "abcd[bddb]xyyx"
                , "aaaa[qwer]tyui"
                , "ioxxoj[asdfgh]zxcvbn" ]

test_part_2 = [  "aba[bab]xyz"
                ,"xyx[xyx]xyx"
                ,"aaa[kek]eke"
                ,"zazbz[bzb]cdb"
                ,"zazbzttaattabcb[bzb]cdb[cbc]"  ]


splitBrackets :: String -> ([String], [String])
splitBrackets foo = splitBrackets' (Split.split (Split.oneOf "[]") foo) [] []

splitBrackets' :: [String] -> [String] -> [String] -> ([String],[String])
splitBrackets' (x:xs) betweenBrackets outsideBrackets
    | x=="[" = splitBrackets' (drop 2 xs) ((head xs):betweenBrackets) outsideBrackets
    | otherwise = splitBrackets' xs (betweenBrackets) (x:outsideBrackets)
splitBrackets' [] betweenBrackets outsideBrackets = (betweenBrackets, outsideBrackets)


matchPalindrom :: String -> Bool
matchPalindrom (w:x:y:z:xs)
    | x==y && w==z && x/=z = True
    | otherwise = matchPalindrom (x:y:z:xs)
matchPalindrom (x:y:z:[]) = False

filterList :: [String] -> [String]
filterList = filter filterFunction

filterFunction :: String -> Bool
filterFunction test = (not $ any matchPalindrom bar) && (any (matchPalindrom) foo)
    where (bar, foo) = splitBrackets test
testData = lines <$> readFile "input_day_7.txt"

getReversePalindromIn :: [String]->[String]
getReversePalindromIn = map reverseABAPalindrom

isThereSLSPattern :: String -> [Bool]
isThereSLSPattern inputString = map ($ takeABAPalindroms foo) (elem <$> (getReversePalindromIn bar))
    where (bar, foo) = splitBrackets inputString

takeABAPalindroms :: [String] -> [String]
takeABAPalindroms = concatMap takeABAPalindroms'
takeABAPalindroms' :: String -> [String]
takeABAPalindroms' (w:x:y:xs)
    | (w==y) && (w/=x) = (w:x:y:[]):(takeABAPalindroms' (x:y:xs))
    | otherwise = (takeABAPalindroms' (x:y:xs))
takeABAPalindroms' (x:_) = []

reverseABAPalindrom :: String ->String
reverseABAPalindrom (x:y:z:[]) = (y:x:y:[])

filterABAPalindroms :: [String] -> [([String],[String])]
filterABAPalindroms (x:xs)
    | palIn /= [] && palOut /= [] = ((palIn,palOut):filterABAPalindroms xs)
    | otherwise = filterABAPalindroms xs
    where {
        (inside, outside) = splitBrackets x;
        (palIn, palOut) = (takeABAPalindroms inside, map reverseABAPalindrom $ takeABAPalindroms outside)
    }
filterABAPalindroms [] = []

anyFromFirstInSecond :: ([String],[String])->Bool
anyFromFirstInSecond (a, b)= or(map (\x -> x `elem` b) a)


result1 :: IO Int
result1 = length . filterList <$> testData

result2 :: IO Int
result2 = length . filter anyFromFirstInSecond . filterABAPalindroms <$> testData
