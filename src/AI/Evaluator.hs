module AI.Evaluator where


import Board.Symbol
import Board.Board
import Data.List

gameFinish :: (Board b) => Int -> b -> Symbol
gameFinish len b
  | rowWin /= E = rowWin
  | colWin /= E = colWin
  | diagWin /= E = diagWin
  | otherwise = E
  where
    wins = containsWin len
    rowWin = lazyTraverse $ allRows b
    colWin = lazyTraverse $ allCols b
    diagWin = lazyTraverse $ allDiagonals b

    lazyTraverse ::  [[Symbol]] -> Symbol
    lazyTraverse [] = E
    lazyTraverse (l:ls)
      |  x == E    = lazyTraverse ls
      | otherwise = x
      where
        x = wins l


isTerminal :: (Board b) => Int -> b -> Bool
isTerminal _ b | isFull b   =  True
isTerminal l b              = gameFinish l b /= E




containsWin :: Int -> [Symbol] -> Symbol
containsWin len line = f line 0 0
  where
     --enough consecutive symbols have been found
    f _ xs os
      | xs >= len = X
      | os >= len = O
    -- end of list without win means draw on this line
    f [] _ _ = E
    -- recursive case, add the current symbol to its count 
    f (E:ys) _ _ = f ys 0 0
    f (X:ys) xs _ = f ys (xs+1) 0
    f (O:ys) _ os = f ys 0 (os+1)


staticEval :: (Board b) => Int -> b -> Int
staticEval _ b =
  sum (map evalLine rows) +
  sum (map evalLine cols) +
  sum (map evalLine diags)
  where
    rows = allRows b
    cols = allCols b
    diags = allDiagonals b


evalLine :: [Symbol] -> Int
evalLine line = sum $ map evalGroup groups
  where
    groups =
      map (\e -> (length e, head e))
      $ filter (\g -> (head g) /= E) $ group line :: [(Int, Symbol)]


evalGroup :: (Int, Symbol) -> Int
evalGroup (1, s) = evalSymb s
evalGroup (2, s) = 5 * evalSymb s
evalGroup (3, s) = 40 * evalSymb s
evalGroup (4, s) = 200 * evalSymb s
evalGroup (5, s) = 10000000 * evalSymb s
evalGroup _ = 0

evalSymb :: Symbol -> Int
evalSymb X = 1
evalSymb O = -1
evalSymb E = 0

