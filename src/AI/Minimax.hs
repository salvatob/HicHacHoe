module AI.Minimax where

import Board.Symbol
import Board.Board
import Board.MyBoardInstance
import AI.Evaluator
import Data.List (maximumBy)
import Data.Ord (comparing)

getEval :: Symbol -> Int
getEval E = 0
getEval X = 1000
getEval O = -1000

currSymbol :: Bool -> Symbol
currSymbol True = X
currSymbol False = O

isSymbolMaxing :: Symbol -> Bool
isSymbolMaxing X = True
isSymbolMaxing O = False
isSymbolMaxing s = error $ "Symbol " ++ show s ++ "is neither max OR min"

evalTTT :: MyBoard -> Int
evalTTT b = getEval $ gameFinish 3 b

-- getBestMove (length to win) state (curr player) = next state
getBestMove :: (Board b) => Int -> b -> Bool -> b
getBestMove len b maxi = snd $
      maximumBy comp movePairs
        where
          nextMoves = nextStates (currSymbol maxi) b
        
          currMoveVal = minimax len 3 (not maxi)

          movePairs = [(currMoveVal m, m) | m <- nextMoves] -- :: [(Int, b)]

          comp :: (Int, b) -> (Int, b) -> Ordering
          comp (x1, _) (x2, _) 
            | maxi        = compare x1 x2
            | otherwise   = compare x2 x1
  


-- depth heuristic encourages player to win in as few moves as possible
-- the more depth is left, the more score the player gets

-- minimax (length to win) state depth maximizing = maximin value
minimax :: Board b => Int -> Int -> Bool -> b -> Int
minimax len depth maxi b
  | depth <= 0 || isTerminal len b  = getEval ter + depthHeuristic
  where
     ter = gameFinish len b
    --  depthHeuristic = if maxi then depth else -depth
     depthHeuristic = 0


minimax len d maxi b
  | maxi =
    maximum [minimax len (d-1) False board | board <- nextBoards]
  | not maxi =
    minimum [minimax len (d-1) True board | board <- nextBoards]

  | otherwise = error "How the fck could there be anything else that bool or not bool. The pattern matching police sucks"
  where
    nextBoards = nextStates (currSymbol maxi) b

