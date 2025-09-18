module Board.BigBoard where

import Board.Board
import Board.MyBoardInstance
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
      isRight = c == getWidth (BigBoard b) -1
      isUp = r == 0
      isDown = r == getHeight (BigBoard b) -1


  printBoard (BigBoard b) = do putStrLn $ showBoard b

  empty = BigBoard $ replicate 5 $ replicate 5 E

  getS (r, c) (BigBoard b) = (b !! r) !! c

  getRow i (BigBoard b) = b !! i

  getCol i (BigBoard b) = map ( !! i ) b

  getWidth (BigBoard b) = length $ head b
  getHeight (BigBoard b) = length b

  rowIndices b = [0..(getHeight b -1)]
  colIndices b = [0..(getWidth b -1)]

  allRows (BigBoard b) = b

  allCols b =
    [getCol j b | j <- colIndices b]

  allDiagonals (BigBoard b) = diagonalsTLBR b ++ diagonalsTRBL b

  nextStates s (BigBoard b) =
    foldl
    (\acc coords->
      if getS coords (BigBoard b) /= E then acc
      else BigBoard (replaceAtMatrix coords s b) : acc)
    []
    coordinates
    where
      coordinates = [(i,j) | i <- rowIndices (BigBoard b), j <- colIndices (BigBoard b)]

  isFull (BigBoard b) = all (notElem E) b



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

