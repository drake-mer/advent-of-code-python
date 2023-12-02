{-
 - Here, the problem is to parse the input string and to deflate it completely.
 - To deflate the string for the second part of the problem, scrapString2
 - provide a slightly optimized answer but still brute-force.
 -
 - A good answer would be to build a tree for each lexeme. The folding
 - of this tree would provide the answer in an extremely efficient fashion.
 -}

inputData = readFile "input_day_9.txt"

splitAt' :: Eq a => a -> [a] -> ([a], [a])
splitAt' x foo = ( (takeWhile (/=x) foo), tail $ dropWhile (/=x) foo)

scrapString1 :: String -> String
scrapString1 (x:xs)
    | x=='(' = duplicated ++ (scrapString1 newString)
    | otherwise = x:(scrapString1 xs)
    where {
        info = getInfo (takeWhile (/=')') xs);
        (duplicated, newString) = duplicateString info (tail $ dropWhile (/=')') xs)
    }
scrapString1 [] = []

scrapString2 :: String -> Int
scrapString2 foo = scrapString2' foo 0

scrapString2' :: String -> Int -> Int
scrapString2' (x:xs) n
    | x=='(' = if '(' `elem` duplicated then scrapString2' (duplicated ++ remaining) n else scrapString2' remaining $! (n+(a*b))
    | otherwise = scrapString2' (dropWhile (/='(') xs) $!(n+1+length(takeWhile (/='(') xs))
    where {
        (duplicated, remaining) = duplicateString (a,b) $ ( tail $ dropWhile (/=')') xs);
        (a,b)=(getInfo (takeWhile (/=')') xs));
    }
scrapString2' [] n = n


duplicateString :: (Int, Int) -> String -> (String, String)
duplicateString (a, b) daString = (concat (take b $ repeat (take a daString)),(drop a daString))

getInfo :: String -> (Int, Int)
getInfo x = (read a, read b) where (a,b) = splitAt' 'x' x
