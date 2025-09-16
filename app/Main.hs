module Main where

import Board.Symbol
import Board.Board
import Board.MyBoardInstance
import AI.Evaluator
import AI.Minimax
import ConsoleInterface
import AI.Minimax (getBestMove)
import Board.Board (Board(printBoard))
import AI.Evaluator (gameFinish)
import Board.Symbol (renderSymbol)




toList :: MyBoard -> [[Symbol]]
toList (MyBoard b) = b

main :: IO ()
main = do

  let b0 = empty 3 3 :: MyBoard
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
  nextMove <- playerMove O b -- :: Board
  result <-  play nextMove False
  return result


-- player is always O
playerMove :: (Board b) => Symbol -> b -> IO b
playerMove s b = do
  putStrLn $ "Please input your next " ++ show s ++" move:"
  move <- readNextMove :: IO Coords
  let newBoard = placeS move b s
  printBoard newBoard
  putStrLn "Computer playing..."
  return newBoard

computerMove :: (Board b) => Symbol -> b -> b
computerMove s b = getBestMove 3 b True





