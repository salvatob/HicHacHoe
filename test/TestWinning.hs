module TestWinning where

import Test.HUnit


import Board.SimpleBoard
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
    ,"test grid no win " ~: gameFinish 3 ( SimpleBoard [
      [X,O,X],
      [O,E,O],
      [X,O,X]
    ])~=? E
    ,"test grid row win" ~: gameFinish 3 ( SimpleBoard [
      [X,X,X],
      [O,E,O],
      [X,E,X]
    ])~=? X
    ,"test grid col win" ~: gameFinish 3 ( SimpleBoard [
      [X,E,O],
      [O,E,O],
      [X,E,O]
    ])~=? O
    ,"test grid main diag win" ~: gameFinish 3 ( SimpleBoard [
      [X,E,O],
      [O,X,O],
      [X,E,X]
    ])~=? X
    ,"test grid oppo diag win" ~: gameFinish 3 ( SimpleBoard [
      [X,E,O],
      [O,O,E],
      [O,E,E]
    ])~=? O
    ,"test grid 4*4 row of 3" ~: X ~=? gameFinish 3 ( SimpleBoard [
      [O,O,E,X],
      [X,X,X,X],
      [O,E,E,O],
      [O,E,E,O]
    ]) 
    ,"test grid 4*4 row of 4" ~: X ~=? gameFinish 4 ( SimpleBoard [
      [O,O,E,X],
      [X,X,X,X],
      [O,E,E,O],
      [O,E,E,O]
    ]) 
  ]