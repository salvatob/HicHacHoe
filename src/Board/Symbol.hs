module Board.Symbol where

data Symbol = O | X | E
  deriving (Show, Eq)

renderSymbol :: Symbol -> String
renderSymbol O = "O"
renderSymbol X = "X"
renderSymbol E = "."
