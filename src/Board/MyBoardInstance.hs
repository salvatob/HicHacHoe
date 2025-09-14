{-# LANGUAGE InstanceSigs #-}
module Board.MyBoardInstance where

import Board.Board
import Board.Symbol

newtype MyBoard = MyBoard [[Symbol]]

instance Board MyBoard where
  printBoard :: MyBoard -> IO ()
  printBoard b = do putStrLn $ showMyBoard b
  
  empty :: Int -> Int -> MyBoard
  empty r c = MyBoard $ replicate r $ replicate c E
  
  placeS :: (Int, Int) -> MyBoard -> Symbol -> MyBoard
  placeS (r, c) (MyBoard b) s = 
    MyBoard (replaceAt c (replaceAt r s (b !! c)) b)

  getS :: (Int, Int) -> MyBoard -> Symbol
  getS (r, c) (MyBoard b) = (b !! c) !! r

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i val xs =
  take i xs ++ [val] ++ drop (i+1) xs


showMyBoard :: MyBoard -> String
-- showMyBoard (MyBoard []) = ""
showMyBoard (MyBoard b) = unlines $ map (map renderSymbol) b

