module Board.Board (Board(..)) where


class (Show a) => Board a where
  -- show ::  a -> String
  printBoard ::  a -> IO ()
  empty :: Int -> Int -> a