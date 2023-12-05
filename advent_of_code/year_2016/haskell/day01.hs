import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
data Step = R Int | L Int deriving (Show, Eq)
data Direction = S | W | N | E
type Vec = (Int, Int)
input = [R 3, L 5, R 1, R 2, L 5, R 2, R 3, L 2, L 5, R 5, L 4, L 3, R 5,
         L 1, R 3, R 4, R 1, L 3, R 3, L 2, L 5, L 2, R 4, R 5, R 5, L 4,
         L 3, L 3, R 4, R 4, R 5, L 5, L 3, R 2, R 2, L 3, L 4, L 5, R 1,
         R 3, L 3, R 2, L 3, R 5, L 194, L 2, L 5, R 2, R 1, R 1, L 1, L 5,
         L 4, R 4, R 2, R 2, L 4, L 1, R 2, R 53, R 3, L 5, R 72, R 2, L 5,
         R 3, L 4, R 187, L 4, L 5, L 2, R 1, R 3, R 5, L 4, L 4, R 2, R 5,
         L 5, L 4, L 3, R 5, L 2, R 1, R 1, R 4, L 1, R 2, L 3, R 5, L 4,
         R 2, L 3, R 1, L 4, R 4, L 1, L 2, R 3, L 1, L 1, R 4, R 3, L 4,
         R 2, R 5, L 2, L 3, L 3, L 1, R 3, R 5, R 2, R 3, R 1, R 2, L 1, L 4,
         L 5, L 2, R 4, R 5, L 2, R 4, R 4, L 3, R 2, R 1, L 4, R 3, L 3, L 4,
         L 3, L 1, R 3, L 2, R 2, L 4, L 4, L 5, R 3, R 5, R 3, L 2, R 5, L 2,
         L 1, L 5, L 1, R 2, R 4, L 5, R 2, L 4, L 5, L 4, L 5,
         L 2, L 5, L 4, R 5, R 3, R 2, R 2, L 3, R 3, L 2, L 5]

input_2 = [R 2, L 3]
input_3 = [R 2, R 2, R 2]
input_4 = [R 5, L 5, R 5, R 3]
input_5 = [R 8, R 4, R 4, R 8]
manhattanDist :: (Int, Int) -> Int
manhattanDist (x,y) = abs x + abs y
getAnswer :: [Step] -> Int
getAnswer theInput = manhattanDist $ Prelude.foldl addStep (0,0) (mapStepsToVecs N theInput)
getNextDir :: Step -> Direction -> Direction
getNextDir (R _) dir =
  case dir of S -> W
              W -> N
              N -> E
              E -> S

getNextDir (L _) dir =
  case dir of S -> E
              E -> N
              N -> W
              W -> S

mapDirToCoord :: Direction -> Int -> (Int, Int)
mapDirToCoord S n = (0, -n)
mapDirToCoord N n = (0,n)
mapDirToCoord E n = (n,0)
mapDirToCoord W n = ( -n, 0)

addStep :: Vec -> Vec -> Vec
addStep (x, y) (x', y') = (x+x', y+y')
getStepNumber :: Step -> Int
getStepNumber (L n) = n
getStepNumber (R n) = n
mapStepsToVecs :: Direction -> [Step] -> [(Int, Int)]
mapStepsToVecs direction (x:xs) = (mapDirToCoord nextDir n):(mapStepsToVecs nextDir xs)
  where nextDir = getNextDir x direction; n=getStepNumber x
mapStepsToVecs _ [] = []

unfoldPath :: [Vec] -> [Vec]
unfoldPath [] = []
unfoldPath (x:xs) | isUnitary x = (x:unfoldPath xs)
                  | otherwise = unitX:(unfoldPath (next:xs))
                  where (unitX, next) = Maybe.fromJust (splitPath x)

isUnitary x = manhattanDist x == 1
splitPath :: Vec -> Maybe (Vec, Vec)
splitPath (x, y)
  | x==0 && y>1 = Just ((0,1),(0,y-1))
  | x==0 && y<(-1) = Just ((0,-1),(0, y+1))
  | y==0 && x>1 = Just ((1,0),(x-1,0))
  | y==0 && x<(-1) = Just ((-1,0),(x+1,0))
  | otherwise = Nothing

getAnswer' :: [Step] -> Maybe Int
getAnswer' theInput = manhattanDist <$> getFirstVisitedTwice unfoldedPath
  where unfoldedPath = scanl1 addStep $ unfoldPath $ mapStepsToVecs (N) theInput

getFirstVisitedTwice :: [(Int,Int)] -> Maybe (Int,Int)
getFirstVisitedTwice foo = getFirstVisitedTwice' Set.empty foo
getFirstVisitedTwice' theSet [] = Nothing
getFirstVisitedTwice' theSet (x:xs)
  | Set.member x theSet = Just x
  | otherwise = getFirstVisitedTwice' (Set.insert x theSet) xs
