module AI.Minimax where

import Board.Symbol
import Board.Board
import Board.MyBoardInstance
import AI.Evaluator


getEval :: Symbol -> Int
getEval E = 0
getEval X = 1000
getEval O = -1000

currSymbol :: Bool -> Symbol
currSymbol True = X
currSymbol False = O

evalTTT :: MyBoard -> Int
evalTTT b = getEval $ isTerminal 3 b

-- getBestMove (length to win) state (curr player) = next state
getBestMove :: (Board b) => Int -> b -> Bool -> b
getBestMove len b maxi = traverseMoves nextMoves nullValue (head nextMoves)
  where
    nextMoves = nextStates (currSymbol maxi) b
    nullValue = if maxi then minBound else maxBound :: Int


    traverseMoves :: (Board b) => [b] -> Int -> b -> b
    traverseMoves [] _ bestMove = bestMove
    traverseMoves (move:bs) bestScore bestMove
        |  currMoveVal > bestScore  = traverseMoves bs currMoveVal move
        |  otherwise                = traverseMoves bs bestScore   bestMove
      where
        -- currMoveVal = 0
        currMoveVal = minimax len move 3 maxi  

-- minimax (length to win) state depth maximizing = maximin value
minimax :: Board b => Int -> b -> Int -> Bool -> Int
minimax len b depth _ 
  | depth <= 0 || ter /= E  = getEval $ isTerminal len b
  where
     ter = isTerminal len b

minimax len b d maxi
  | maxi = 
    maximum [minimax len board (d-1) False | board <- nextBoards]
  | not maxi = 
    minimum [minimax len board (d-1) True | board <- nextBoards]

  | otherwise = error "How the fck could there be anything else that bool or not bool. The pattern matching police sucks"
  where 
    nextBoards = nextStates (currSymbol maxi) b

