module AI.Evaluator where


import Board.Symbol
import Board.Board

isTerminal :: (Board b) => Int -> b -> Symbol
isTerminal len b
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


