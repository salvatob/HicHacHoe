module Board.Symbol where

data Symbol = O | X | E
  deriving (Show)

renderSymbol :: Symbol -> Char
renderSymbol O = 'O'
renderSymbol X = 'X'
renderSymbol E = '.'
