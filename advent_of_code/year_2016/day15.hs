
data Disc = Disc InitPosition PositionRange Index
    deriving (Eq, Show)
type InitPosition = Int
type PositionRange = Int
type Position = Int
type Index = Int
type Time = Int

positionAtTime :: Disc -> Time -> Position
positionAtTime (Disc initPos posRange index) time = (initPos + time + index) `mod` posRange

readDisc :: [String] -> [Disc]
readDisc = map readOneDisc 

readOneDisc :: String -> Disc
readOneDisc input = Disc initPos posRange numDisc
    where numDisc = (read (tail (splittedString!!1)))
          posRange = read (splittedString!!3)
          initPos = read (init (last splittedString))
          splittedString = words input


myDiscs :: IO [Disc]
myDiscs = (readDisc.lines) <$> (readFile "input_day_15.txt")

isAnswer :: Time -> [Disc] -> Bool
isAnswer time input = all (==0) (map ((flip positionAtTime) time) input)

findAnswer :: [Disc] -> Time
findAnswer input = head (filter (flip isAnswer input) [0..])

findAnswer2 :: [Disc] -> Time
findAnswer2 input = head (filter (flip isAnswer (input++[(Disc 0 11 7)])) [0..])
