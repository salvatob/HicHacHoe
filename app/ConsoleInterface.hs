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


-- read :: IO (Int, Int)
-- read = do
--   putStrLn "Please "
