module ConsoleInterface where

import Data.Char

type Coords = (Int, Int)

parseDigits :: String -> Int
parseDigits d = read d -1 

parseChar :: Char -> Int
parseChar c = ord c - 97


parseLine :: String -> Coords
parseLine line = (parseDigits r, parseChar c)
  where
    c = head $ takeWhile isLower line
    r = takeWhile isDigit $ dropWhile isLower line


readNextMove :: IO Coords
readNextMove = do
  line <- getLine
  return $ parseLine line


queryFirstPlayer :: IO Bool
queryFirstPlayer = do
  putStrLn "Do you want to play first? (y/n)"

  answer <- getLine

  if elem 'y' answer
    then return True
  else
    if elem 'n' answer
      then return False
    else error "You have not answered correctly (y/n)"



data Opponent = Human | Computer
  -- deriving EQ

queryOpponent :: IO Opponent
queryOpponent = do
  putStrLn "Who do you want to play against? ('1'/'2')"
  putStrLn "1. another human player"
  putStrLn "2. AI player"

  answer <- getLine

  if elem '1' answer
    then return Human
  else
    if elem '2' answer
      then return Computer
    else error "You have not answered correctly (1/2)"

data GameMode = TTT | ConnectX
  -- deriving (EQ)


queryGameMode :: IO GameMode
queryGameMode = do
  putStrLn "Which game do you want to play? ('1'/'2')"
  putStrLn "1. standard TicTacToe"
  putStrLn "2. larger version - connect X"

  answer <- getLine

  if elem '1' answer
    then return TTT
  else
    if elem '2' answer
      then return ConnectX
    else error "You have not answered correctly (1/2)"

-- queryGameSize :: IO Int
-- queryGameSize = do
--   putStrLn "How many symbols to connect?"
--   answer <- getLine

--   let num = takeWhile isDigit $ dropWhile (not . isDigit) answer
  
--   let res = read num :: Int
--   return res