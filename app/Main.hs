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
  -- let b0 = empty :: MyBoard
  printBoard b0

  -- let b1 = placeS ((2,2)) b0 X
  -- printBoard b1
  
  -- let b2 = placeS ((1,4)) b1 O
  -- printBoard b2

  result <- play b0 False
  putStrLn $ renderSymbol result ++ " has won"

  return ()



askGamemode :: (Board b) => IO (b, Int)
askGamemode = do
  gamemode <- queryGameMode :: IO GameMode

  case gamemode of
    TTT -> do
      let ttt = empty :: MyBoard
      return (ttt, 3)

    ConnectX -> do
      winLength <- queryGameSize :: IO Int
      let mode = empty :: BigBoard
      return (mode, winLength)

  error "Invalid game mode has been selected"
  -- return ()


play :: (Board b) => b -> Bool -> IO Symbol
play b _ | isTerminal winLength b = do return $ gameFinish winLength b
  where
    winLength = 4

play b False = do
  let nextMove = computerMove O b -- :: Board
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





