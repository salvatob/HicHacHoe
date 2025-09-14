module Main where

import  Board.MyBoardInstance
import  Board.Board

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ""
  
  let b0 = empty 3 3 :: MyBoard
  printBoard b0

  putStrLn ""
  putStrLn "Hello, Haskell!"
