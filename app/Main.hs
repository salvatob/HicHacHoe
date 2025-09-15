module Main where

import Board.Symbol
import  Board.Board
import  Board.MyBoardInstance
import AI.Evaluator



toList :: MyBoard -> [[Symbol]]
toList (MyBoard b) = b

main :: IO ()
main = do


  -- return ()
  putStrLn "Hello, Haskell!"
  putStrLn ""
  -- let b0 = MyBoard [[O,E],[E,X]]
  let b0 = empty 3 3 :: MyBoard
  let b1 = placeS (2,0) b0 X
  let b2 = placeS (1,1) b1 X
  let b3 = placeS (0,2) b2 X
  printBoard b3

  -- let h = getS (1,2) b3
  -- print (renderSymbol h)
  -- let h2 = getS (2,1) b3
  -- print (renderSymbol h2)

  let r1 = allDiagonals b3
  print r1
