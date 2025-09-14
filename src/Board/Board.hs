module Board.Board where

import Board.Symbol ( Symbol )


class  Board b where
  printBoard ::  b -> IO ()
  empty :: Int -> Int -> b
  placeS :: (Int, Int) -> b -> Symbol -> b
  getS ::  (Int, Int) -> b -> Symbol