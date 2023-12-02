type Tile = Char

input :: [Tile]
input = concat [ ".^..^....^....^^."
                ,"^^.^.^^.^.....^.^"
                ,"..^...^^^^^^.^^^^"
                ,".^.^^^^^^^.^^^^^."
                ,".^.^^^.^^..^.^^.^"
                ,"....^.^...^^.^." ]

inputSize :: Int
inputSize = length input

isTrap :: Tile -> Bool 
isTrap = (=='^')

isSafe :: (Tile, Tile, Tile) -> Bool
isSafe (left, center, right) 
    | (isTrap left) && (isTrap center) && (not $ isTrap right) = False
    | (isTrap center) && (isTrap right) && (not $ isTrap left) = False
    | (isTrap left) && (not $ isTrap right) && (not $ isTrap center) = False
    | (isTrap right) && (not $ isTrap left) && (not $ isTrap center) = False
    | otherwise = True
    
getTile :: (Tile, Tile, Tile) -> Tile
getTile x = if (isSafe x) then '.' else '^'

buildRow :: Tile -> [Tile] -> [Tile]
buildRow left (center:follow)
    | follow == [] = [getTile (left, center, '.')]
    | otherwise = (getTile (left, center, (head follow))):(buildRow center follow)

nextRow :: [Tile] -> [Tile]
nextRow = buildRow '.'
answer1 = take 40 (iterate nextRow input)
answer2 = answer2' input 400000 0
answer2' :: [Tile] -> Int -> Int -> Int
answer2' row maxRow acc 
    | maxRow == 0 = acc
    | otherwise = answer2' (nextRow row) (maxRow-1) ((length $! filter (=='.') row) + acc)


