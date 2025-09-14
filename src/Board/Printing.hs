module Board.Printing (someFunc) where

import Board.Board 


data Symbol = O | X | E
  deriving (Show)


newtype MyBoard = MyBoard [Symbol]
  deriving (Show)

instance Board MyBoard where
  -- show (MyBoard b) = printBoard b
  printBoard (MyBoard b) = printMyBoard b

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- printMyBoard :: MyBoard -> IO ()
printMyBoard :: [Symbol] -> IO ()
printMyBoard board
-- printMyBoard (MyBoard board)
  | not $ checkSize board = error "Board is not of size 9"
  | otherwise =
    do
      let (a,b,c) = thirds board
      printRow a
      putStrLn ""

      putStrLn "-----"
      printRow b
      putStrLn ""

      putStrLn "-----"
      printRow c
      putStrLn ""


printRow ::  Show a => [a] -> IO ()
printRow [] = return ()
printRow [x] = putStr (Prelude.show x)
printRow (x:xs) =
  do
    putStr (Prelude.show x)
    putChar '|'
    printRow xs


thirds :: [a] -> ([a], [a], [a])
thirds l
  | len < 3 = error "Not enough items"
  | mod len 3 /= 0 = error "Items are not divisible by three"
  | otherwise =
    (
      take t l,
      take t (drop t l),
      take t $ drop (t*2) l
    )
    where
      len = length l
      t = div len 3


checkSize :: [a] -> Bool
checkSize board = length board == 9