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



replaceAt :: Int -> a -> [a] -> [a]
replaceAt i val xs =
  take i xs ++ [val] ++ drop (i+1) xs

replaceAtMatrix :: (Int, Int) -> a -> [[a]] -> [[a]]
replaceAtMatrix (i, j) val xs = replaceAt i (replaceAt j val (xs !! i)) xs
