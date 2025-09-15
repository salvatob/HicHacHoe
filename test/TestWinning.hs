module TestWinning where

import Test.HUnit


import Board.MyBoardInstance
-- import Board.Board
import Board.Symbol
import AI.Evaluator
-- import AI.Minimax


tests :: Test
tests = TestList
  [ 
     "test 1" ~: containsWin 3 [X,X,X] ~=? X
    ,"test 2" ~: containsWin 2 [X,X,X] ~=? X
    ,"test 3" ~: containsWin 4 [X,X,X] ~=? E
    ,"test 4" ~: containsWin 2 [X,O,X] ~=? E
    ,"test grid no win " ~: isTerminal 3 ( MyBoard [
      [X,O,X],
      [O,E,O],
      [X,O,X]
    ])~=? E
    ,"test grid row win" ~: isTerminal 3 ( MyBoard [
      [X,X,X],
      [O,E,O],
      [X,E,X]
    ])~=? X
    ,"test grid col win" ~: isTerminal 3 ( MyBoard [
      [X,E,O],
      [O,E,O],
      [X,E,O]
    ])~=? O
    ,"test grid main diag win" ~: isTerminal 3 ( MyBoard [
      [X,E,O],
      [O,X,O],
      [X,E,X]
    ])~=? X
    ,"test grid oppo diag win" ~: isTerminal 3 ( MyBoard [
      [X,E,O],
      [O,O,E],
      [O,E,E]
    ])~=? O
  ]