-- {-# HLINT ignore "Redundant return" #-}
module Main where

import Board.Board
import Board.MyBoardInstance
import Board.BigBoard
import AI.Evaluator
import AI.Minimax
import ConsoleInterface
import Board.Symbol

main :: IO ()
main = do

  let b0 = empty :: BigBoard 

  printBoard b0

  -- let options = nextStates O b0

  -- mapM_ printBoard options

  res <- playTTT b0 True

  return ()



askGamemode ::  IO (GameMode, Opponent)
askGamemode = do
  gamemode <- queryGameMode :: IO GameMode
  opponent <- queryOpponent :: IO Opponent

  return (gamemode, opponent)

-- play :: (Board b) => 
-- play  player1MoveGen player2MoveGen


playTTT :: (Board b) => b -> Bool -> IO Symbol
playTTT b _ | isTerminal winLength b = do return $ gameFinish winLength b
  where
    winLength = 5


playTTT b False = do
  let nextMove = computerMove O b -- :: Board
  printBoard nextMove
  result <-  playTTT nextMove True
  return result

playTTT b True = do
  nextMove <- playerMove X b -- :: Board
  result <-  playTTT nextMove False
  return result


playerMove :: (Board b) => Symbol -> b -> IO b
playerMove s b = do
  putStrLn $ "Please input your next " ++ (renderSymbol s) ++" move:"
  move <- readNextMove :: IO Coords
  let newBoard = placeS move b s
  printBoard newBoard
  putStrLn "Computer playing..."
  return newBoard

computerMove :: (Board b) => Symbol -> b -> b
computerMove s b = getBestMove 5 4 b $ isSymbolMaxing s





