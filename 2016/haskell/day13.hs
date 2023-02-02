-- import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import qualified Data.Tree as Tree
import Control.Monad
import Data.Maybe as Maybe

type Labyrinth = [[Coord]]
data Coord = C Int Int deriving ( Show, Eq, Ord )

magic :: Int
magic = 1362

input_target :: Coord
input_target = (C 31 39)

myMap :: [[Coord]]
myMap = [[ (C x y) | x<-[0..] ] | y<-[0..] ]

myMap' :: [[Coord]]
myMap' = [[ (C x y) | x<-[0..50] ] | y<-[0..50] ]

type PathMaker = (Coord, Set.Set Coord)

getSubLevel :: PathMaker -> (Coord, [PathMaker])
getSubLevel (c, v) = (c, (zip (getAdjacentNodes c v) (repeat (Set.insert c v))))

explorationTree :: Tree.Tree Coord
explorationTree = Tree.unfoldTree getSubLevel(C 1 1, Set.empty)

flat n (Tree.Node a nodeList)
    | null nodeList || n==0 = [[a]]
    | otherwise = map (a:) (concatMap (flat (n-1)) nodeList)

lengthPath :: Tree.Tree Coord -> Coord -> Int
lengthPath tree target = length $ takeWhile (not . (elem target)) (Tree.levels tree)

isOpenSpace :: Coord -> Bool
isOpenSpace (C x y) = even . numberOfOne . binaryForm $ indicatorFunction x y

{- indicator function to help determine wall and openspace position -}
indicatorFunction :: Int -> Int -> Int
indicatorFunction x y = m + x*x + 3*x + 2*x*y + y + y*y
    where m=magic

{- convert an integer into its binary form -}
binaryForm :: Int -> [Int]
binaryForm x
    | x==0 = []
    | x `mod` 2 == 0 = (0:(binaryForm (x `div` 2)))
    | otherwise = (1:(binaryForm ((x-1) `div` 2)))

numberOfOne :: [Int] -> Int
numberOfOne = length . filter (==1)

getAdjacentNodes :: Coord -> Set.Set Coord -> [Coord]
getAdjacentNodes (C r c) theSet = do
    (c', r') <- [(c+1,r),(c,r+1),(c-1,r),(c,r-1)]
    guard ( and [0<=c', r'>=0, isOpenSpace (C r' c'), not((C r' c') `Set.member` theSet)] )
    return (C r' c')
