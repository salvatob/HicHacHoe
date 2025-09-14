module AI.Evaluator where


import Board.Symbol

-- isTerminal :: (Board b) => Int -> b -> Symbol 
-- isTerminal l (MyBoard b) = _



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




-- checkCols :: (Board b) => Int -> b -> Symbol 
-- checkDiagonals :: (Board b) => Int -> b -> Symbol 

-- evaluate :: Int -> MyBoard -> Int