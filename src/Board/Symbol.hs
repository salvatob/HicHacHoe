module Board.Symbol where

data Symbol = O | X | E
  deriving (Show, Eq)

renderSymbol :: Symbol -> Char
renderSymbol O = 'O'
renderSymbol X = 'X'
renderSymbol E = '.'

-- instance Show [Symbol] where
