import qualified Data.Maybe as Mb
to3tuple :: [Int] -> (Int,Int,Int)
to3tuple (x:y:z:_) = (x,y,z)

type Triangle = (Int, Int, Int)

data_3 :: IO [Triangle]
data_3 = do
  content <- readFile "input_day_3.txt"
  let triangleRep = lines content
  let trianglesAsListsOfStrings = map words triangleRep
  let trianglesAsListsOfInt = map (map read) trianglesAsListsOfStrings
  let triangleAsTuples = map to3tuple trianglesAsListsOfInt
  return (triangleAsTuples)

data_4 :: IO [Int]
data_4 = do
  content <- readFile "input_day_3.txt"
  let triangleRep = lines content
  let trianglesAsListsOfStrings = map words triangleRep
  let trianglesAsListsOfInt = concatMap (map read) trianglesAsListsOfStrings
  return (trianglesAsListsOfInt)


isPossible (x,y,z)
  | x+y <= z || x+z <= y || y+z <= x = False
  | otherwise = True

triangleFromList :: [Int] -> [Triangle] -> [Triangle]
triangleFromList (x:y:z:xp:yp:zp:xpp:ypp:zpp:xs) trList = triangleFromList xs newTrList
  where newTrList = ((x,xp,xpp):(y,yp,ypp):(z,zp,zpp):trList)
triangleFromList [] trList = trList

result_1 = length <$> filter isPossible <$> data_3
result_2 = length <$> filter isPossible <$> flip triangleFromList [] <$> data_4
