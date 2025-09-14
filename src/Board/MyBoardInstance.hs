module Board.MyBoardInstance where

import Board.Board
import Board.Symbol

newtype MyBoard = MyBoard [[Symbol]]

instance Board MyBoard where
  printBoard b = do putStrLn $ showMyBoard b

  empty r c = MyBoard $ replicate r $ replicate c E

  placeS (r, c) (MyBoard b) s =
    MyBoard (replaceAt r (replaceAt c s (b !! r)) b)

  getS (r, c) (MyBoard b) = (b !! r) !! c

  getRow i (MyBoard b) = b !! i

  getCol i (MyBoard b) = map ( !! i ) b

  getWidth (MyBoard b) = length $ head b
  getHeight (MyBoard b) = length b

  allRows (MyBoard b) = b

  allCols (MyBoard b) =
    [getCol i (MyBoard b) | i <- [0..(getWidth (MyBoard b) -1)]]

  allDiagonals (MyBoard b) = error "AllDiagonals not implemented yet"






replaceAt :: Int -> a -> [a] -> [a]
replaceAt i val xs =
  take i xs ++ [val] ++ drop (i+1) xs


showMyBoard :: MyBoard -> String
-- showMyBoard (MyBoard []) = ""
showMyBoard (MyBoard b) = unlines $ map (map renderSymbol) b

