{-# LANGUAGE InstanceSigs #-}
module Board.MyBoardInstance (someFunc, MyBoard(..)) where

import Board.Board


data Symbol = O | X | E
  deriving (Show)

renderSymbol :: Symbol -> Char
renderSymbol O = 'O'
renderSymbol X = 'X'
renderSymbol E = '.'

-- instance Show Symbol where
--   show :: Symbol -> String
--   show O = "O"
--   show X = "X"
--   show E = "."

newtype MyBoard = MyBoard [[Symbol]]
  deriving (Show)

instance Board MyBoard where
  -- show (MyBoard b) = Prelude.show b
  printBoard b = do putStrLn $ showMyBoard b
  empty r c = MyBoard $ replicate r $ replicate c E

someFunc :: IO ()
someFunc = putStrLn "someFunc"


showMyBoard :: MyBoard -> String
-- showMyBoard (MyBoard []) = ""
showMyBoard (MyBoard b) = unlines $ map (map renderSymbol) b

