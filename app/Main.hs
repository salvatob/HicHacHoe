{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Main where

import Board.Board
import Board.MyBoardInstance
import AI.Evaluator
import AI.Minimax
import ConsoleInterface
import Board.Symbol


-- toList :: MyBoard -> [[Symbol]]
-- toList (MyBoard b) = b

main :: IO ()
main = do

  let b0 = empty :: MyBoard
  result <- play b0 False
  putStrLn $ renderSymbol result ++ " has won"

  return ()


play :: (Board b) => b -> Bool -> IO Symbol
play b _ | isTerminal 3 b = do return $ gameFinish 3 b

play b False = do
  let nextMove = computerMove X b -- :: Board
  printBoard nextMove
  result <-  play nextMove True
  return result

play b True = do
  nextMove <- playerMove X b -- :: Board
  result <-  play nextMove False
  return result


playerMove :: (Board b) => Symbol -> b -> IO b
playerMove s b = do
  putStrLn $ "Please input your next " ++ show s ++" move:"
  move <- readNextMove :: IO Coords
  let newBoard = placeS move b s
  printBoard newBoard
  putStrLn "Computer playing..."
  return newBoard

computerMove :: (Board b) => Symbol -> b -> b
computerMove s b = getBestMove 3 b $ isSymbolMaxing s





