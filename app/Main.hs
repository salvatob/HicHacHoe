module Main where

import Board.Symbol
import  Board.Board
import  Board.MyBoardInstance


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ""
  
  let b0 = empty 3 3 :: MyBoard
  printBoard b0
  let b1 = placeS (1,0) b0 X

  putStrLn ""
  putStrLn "Hello, Haskell!"
