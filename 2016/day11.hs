import Data.Tree

data GameState = GameState
data Move = Move


solutionTree :: GameState -> 


isMoveValid :: GameState -> Move -> Bool
isMoveValid = undefined

playMove :: GameState -> Move -> GameState
playMove = undefined

isNodeValid :: GameState -> Move -> Bool
isNodeValid gs move
    | any isNodeValid (possibleMove $ isNodeValid (playMove gs move))
    | isMoveValid GameState Move == False = False

isGameComplete :: GameState -> Bool
isGameComplete = undefined

possibleMove :: GameState -> [Move]
possibleMove = undefined

applyMove :: GameState -> Move -> GameState
applyMove = undefined


