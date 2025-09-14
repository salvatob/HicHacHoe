module Main where

import Board.Symbol
import  Board.Board
import  Board.MyBoardInstance
import AI.Evaluator


-- All top-left → bottom-right diagonals
diagonalsTLBR :: [[a]] -> [[a]]
diagonalsTLBR matrix =
    let n = length matrix
        m = if null matrix then 0 else length (head matrix)
        -- generate diagonals starting on first row
        diagFromRow r = [matrix !! (r+k) !! k | k <- [0..min (n-r-1) (m-1)]]
        -- generate diagonals starting on first column
        diagFromCol c = [matrix !! k !! (c+k) | k <- [0..min (n-1) (m-c-1)]]
    in [diagFromRow r | r <- [0..n-1]] ++ [diagFromCol c | c <- [1..m-1]]

-- All top-right → bottom-left diagonals
diagonalsTRBL :: [[a]] -> [[a]]
diagonalsTRBL matrix =
    let n = length matrix
        m = if null matrix then 0 else length (head matrix)
        diagFromRow r = [matrix !! (r+k) !! (m-1-k) | k <- [0..min (n-r-1) (m-1)]]
        diagFromCol c = [matrix !! k !! (c-k) | k <- [0..min (n-1) c]]
    in [diagFromRow r | r <- [0..n-1]] ++ [diagFromCol c | c <- [m-2,m-3..0]]

toList :: MyBoard b => [[Symbol]]
toList (MyBoard b) = b

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

  let t = isTerminal 3 (MyBoard [[X,O,X],[O,X,O],[X,O,X]])
  print t

  let d1 = diagonalsTLBR $ toList b3
  print d1