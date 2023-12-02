import Data.Maybe
import Data.List

type IpRange = (Int, Int)

input = do
    content <- lines <$> readFile "input_day_20.txt"
    return content;

ipRange :: String -> IpRange
ipRange range = (read a, read $ tail b)
    where (a,b) = splitAt splitPos range
          splitPos = fromJust (elemIndex '-' range)


allRanges :: [String] -> [IpRange]
allRanges = map ipRange


minForbidden :: [IpRange] -> Int
minForbidden = fst . lowestRange 
    where lowestRange =  minimumBy (\a b -> compare (fst a) (fst b))

mergeRange :: IpRange -> IpRange -> IpRange
mergeRange (a, b) (c, d) = (min a c, max b d)

isOverlapping :: IpRange -> IpRange -> Bool
isOverlapping (a, b) (c, d) -- we assume always that a<b and c<d
    | (a>=c && a<=d) = True -- a is in (c, d), so overlapping
    | (b>=c && b<=d) = True -- b is in (c, d), so overlapping
    | (c<=b && c>=a) = True -- c is in (a, b), so overlapping
    | (d<=b && d>=a) = True -- d is in (a, b), so overlapping
    | (b == (c-1)) || (d == (a-1)) = True
    | otherwise = False -- in all the other cases, this is False
    


mergeRanges :: [IpRange] -> [IpRange]
mergeRanges (ip1:ip2:otherIp) 
    | isOverlapping ip1 ip2 = mergeRanges ((mergeRange ip1 ip2):otherIp)
    | otherwise = (ip1:(mergeRanges (ip2:otherIp)))
mergeRanges (x:[]) = [x]
mergeRanges [] = [] 

getData :: [String] -> [IpRange]
getData = mergeRanges .sort . allRanges

answer1 :: IO Int
answer1 = (+1) . snd . head . getData <$> input

answer2 :: IO Int
answer2 = allowedIp . getData <$> input

allowedIp :: [IpRange] -> Int
allowedIp = allowedIp' (0, 0) 2

allowedIp' :: IpRange -> Int -> [IpRange] -> Int
allowedIp' (a, b) acc ((c, d):xs)
    | not . null $ xs = allowedIp' (c,d) (acc + (c-b-1)) xs
    | otherwise = acc


