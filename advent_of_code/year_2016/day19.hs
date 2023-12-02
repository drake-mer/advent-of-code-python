import qualified Data.Vector as Vector

input = 3005290

type Circle = Vector.Vector Elve
type Elve = Integer

-- circle :: Circle 
-- circle = Vector.fromList [1..input]

{- The nextPos gives the reached position across the
 - circle from the starting currentPos. Indexing starts
 - at 0. -}
nextPos :: Int -> Int -> Int 
nextPos currentPos size = ((currentPos + (size `div` 2)) ) `mod` size

remove :: Int -> Circle -> Circle
remove pos cercle = ((Vector.++) s e)
    where {
       s = ((Vector.take) pos cercle);
       e = ((Vector.drop) (pos+1) cercle);
    }

emptyCircle :: (Circle, Int) -> (Circle, Int)
emptyCircle (circle, currentPos)
    | currentLength == 1 = (circle, 0)
    | otherwise = emptyCircle (newCircle, newPos)
    where currentLength = (Vector.length circle)
          pos = nextPos currentPos (currentLength)
          newCircle = remove pos circle
          newPos = (currentPos + 1) `mod` (currentLength)
circle :: Circle
circle = Vector.fromList [1..5]

answer :: (Circle, Int)
answer = emptyCircle (Vector.fromList [1..input], 0)
 
