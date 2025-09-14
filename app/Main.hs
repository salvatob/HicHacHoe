module Main where

import Board.Symbol
import  Board.Board
import  Board.MyBoardInstance


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn ""
  -- let b0 = MyBoard [[O,E],[E,X]]
  let b0 = empty 3 3 :: MyBoard
  let b1 = placeS (1,0) b0 X
  let b2 = placeS (1,2) b1 E
  let b3 = placeS (2,1) b2 O
  printBoard b3

  let h = getS (1,2) b3
  print (renderSymbol h)
  let h2 = getS (2,1) b3
  print (renderSymbol h2)

  let r1 = getRow 1 b3 :: [Symbol]
  print r1
  let c1 = getCol 1 b3 :: [Symbol]
  print c1