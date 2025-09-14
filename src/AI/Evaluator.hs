module AI.Evaluator where


import Board.Symbol
import Board.Board

isTerminal :: (Board b) => Int -> b -> Symbol
isTerminal l b
  | rowWin /= E = rowWin
  | colWin /= E = colWin
  -- | diagWin /= E = diagWin
  | otherwise = E
  where 
    wins = containsWin l
    rowWin = lazyTraverse $ allRows b
    colWin = lazyTraverse $ allCols b
    -- diagWin = any wins $ allDiagonals b

    lazyTraverse ::  [[Symbol]] -> Symbol
    lazyTraverse [] = E
    lazyTraverse (line:lines)
      |  x == E    = lazyTraverse lines
      | otherwise = x
      where 
        x = wins line




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



-- evaluate :: Int -> MyBoard -> Int