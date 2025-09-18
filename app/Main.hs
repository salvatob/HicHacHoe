{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
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
  mode <- askGamemode

  _ <- playGame mode

  return ()



askGamemode ::  IO (GameMode, Opponent)
askGamemode = do
  gamemode <- queryGameMode :: IO GameMode
  opponent <- queryOpponent :: IO Opponent

  return (gamemode, opponent)


playGame :: (GameMode, Opponent) -> IO Symbol
playGame (TTT, Computer) = do 
  let b0 = empty :: MyBoard

  printBoard b0
  let p1 = playerMove
  let p2 = computerMoveCX

  res <- playCX p1 p2 b0

  putStrLn $ show res ++ " has won"
  return res

playGame (TTT, Human) = do 
  let b0 = empty :: MyBoard

  printBoard b0
  let p1 = playerMove
  let p2 = playerMove

  res <- playCX p1 p2 b0

  putStrLn $ show res ++ " has won"
  return res

playGame (ConnectX, Computer) = do 
  let b0 = empty :: BigBoard

  printBoard b0
  let p1 = playerMove
  let p2 = computerMoveCX

  res <- playCX p1 p2 b0

  putStrLn $ show res ++ " has won"
  return res

playGame (ConnectX, Human) = do 
  let b0 = empty :: BigBoard

  printBoard b0
  let p1 = playerMove
  let p2 = playerMove

  res <- playCX p1 p2 b0

  putStrLn $ show res ++ " has won"
  return res


playTTT :: (Board b) => (Symbol -> Int -> b -> IO b) -> (Symbol -> Int -> b -> IO b) -> b -> IO Symbol
playTTT p1 p2 initial = do
  result <- play p1 E True initial
  return result

  where
    winLength = 3

    -- play :: (Symbol -> Int -> b -> IO b) -> Symbol -> Bool -> b -> IO Symbol
    play _ _ _ b | isTerminal winLength b = do
        return $ gameFinish winLength b

    play player _ True b = do
      nextMove <- player O winLength b -- :: Board
      printBoard nextMove
      result <- play p2 X False nextMove
      return result

    play player _ False b  = do
      nextMove <- player X winLength b -- :: Board
      printBoard nextMove
      result <- play p1 O True nextMove
      return result



playCX :: (Board b) => (Symbol -> Int -> b -> IO b) -> (Symbol -> Int -> b -> IO b) -> b -> IO Symbol
playCX p1 p2 initial = do
  result <- play p1 E True initial
  return result

  where
    winLength = 5

    -- play :: (Symbol -> Int -> b -> IO b) -> Symbol -> Bool -> b -> IO Symbol
    play _ _ _ b | isTerminal winLength b = do
        return $ gameFinish winLength b

    play player _ True b = do
      nextMove <- player O winLength b -- :: Board
      printBoard nextMove
      result <- play p2 X False nextMove
      return result

    play player _ False b  = do
      nextMove <- player X winLength b -- :: Board
      printBoard nextMove
      result <- play p1 O True nextMove
      return result



playerMove :: (Board b) => Symbol -> Int -> b -> IO b
playerMove s _ b = do
  putStrLn $ "Please input your next " ++ show s ++" move:"
  move <- readNextMove :: IO Coords
  let newBoard = placeS move b s
  -- printBoard newBoard
  return newBoard

computerMoveTTT :: (Board b) => Symbol -> Int -> b -> IO b
computerMoveTTT s l b = do
    putStrLn $ "Computer playing as " ++ show s
    let move = getBestMove (getEval . gameFinish 3) l depth b $ isSymbolMaxing s
    -- printBoard move
    -- putStrLn "Computer has played."
    return move
  where
    depth = 9



computerMoveCX :: (Board b) => Symbol -> Int -> b -> IO b
computerMoveCX s l b = do
    putStrLn "Computer playing..."
    return $ getBestMove (staticEval 5) l depth b $ isSymbolMaxing s
  where
    depth = 4


