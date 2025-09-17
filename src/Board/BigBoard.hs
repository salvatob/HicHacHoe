module Board.BigBoard where

import Board.Board
import Board.Symbol


newtype BigBoard = BigBoard [[Symbol]]

instance Board BigBoard where
    placeS (r, c) (BigBoard b) s = BigBoard $
      (
      (if isRight then resizeRight E else id)  .  
      (if isLeft then resizeLeft E else id) .
      (if isDown then resizeDown E else id) .
      (if isUp then resizeUp E else id))
        placed
      where
        placed = replaceAt r (replaceAt c s (b !! r)) b
        isLeft = c == 0
        isRight = c == getWidth (BigBoard b)
        isUp = r == 0
        isDown = c == getHeight (BigBoard b)

data Direction = UP | DOWN | LEFT | RIGHT


resize :: Direction -> a -> [[a]] -> [[a]]
resize UP = resizeUp
resize DOWN = resizeDown
resize LEFT = resizeLeft
resize RIGHT = resizeRight


resizeDown :: a -> [[a]] -> [[a]]
resizeDown default_ xss = xss ++ [bottomRow]
  where
    width = length $ head xss
    bottomRow =  replicate width default_


resizeUp :: a -> [[a]] -> [[a]]
resizeUp default_ xss = topRow : xss
  where
    width = length $ head xss
    topRow =  replicate width default_


resizeLeft :: a -> [[a]] -> [[a]]
resizeLeft default_ = map (default_ :)


resizeRight :: a -> [[a]] -> [[a]]
resizeRight default_ = map (++[default_])

