module Board.Board where

import Board.Symbol 


class Board b where
  printBoard ::  b -> IO ()
  empty :: b
  placeS :: (Int, Int) -> b -> Symbol -> b
  getS ::  (Int, Int) -> b -> Symbol
  getRow :: Int -> b -> [Symbol]
  getCol :: Int -> b -> [Symbol]
  getWidth :: b -> Int
  rowIndices :: b -> [Int]
  colIndices :: b -> [Int]
  getHeight :: b -> Int
  allRows :: b -> [[Symbol]]
  allCols :: b -> [[Symbol]]
  allDiagonals :: b -> [[Symbol]]
  nextStates :: Symbol -> b -> [b]
  isFull :: b -> Bool
