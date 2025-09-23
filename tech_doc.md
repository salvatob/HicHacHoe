# Technical Documentation

This project is separated into three main parts:

---

### Board
The Board module encapsulates the behavior of the playing board. Its purpose is to abstract away the representation of the board while providing a standard interface that the rest of the system can rely on.

#### Main features:
- Printing the board to stdout.
- Executing moves (placing symbols on the board).
- Generating all legal moves.
- Resizing the board if needed.

Most of the application’s architecture relies on the `Board` typeclass. This allows alternative implementations to be swapped in without changing the rest of the code. The typeclass encapsulates the standard methods while hiding implementation details.

#### The `Symbol` dataclass
For ease of use, I am working with symbol as it's own type. This decision was made purely to catch some bugs early during compilation.
```haskell
data Symbol = O | X | E
  deriving (Show, Eq)
```
In this project, `E` means "empty cell".

#### The `Board` Typeclass

The core of the board logic is the `Board` typeclass, defined in `src/Board/Board.hs`. This typeclass specifies the interface that any board implementation must provide. By using this abstraction, the AI and other modules can operate on any board type that implements this interface, without knowing the details of its internal representation.

The most important functions in the `Board` typeclass are:

```haskell
class Board b where
  printBoard :: b -> IO ()
  empty      :: b
  placeS     :: (Int, Int) -> b -> Symbol -> b
  nextStates :: Symbol -> b -> [b]
  -- ...other methods...
```

Where:
- `printBoard :: b -> IO ()`  
  Prints the current board state to stdout in a human-readable format, with coordinates, so humans can.

- `empty :: b`
  Returns a new, empty instance.

- `placeS :: (Int, Int) -> b -> Symbol -> b`  
  Places a symbol (X or O) at the specified coordinates on the board, returning a new board state.

- `nextStates :: Symbol -> b -> [b]`  
  Generates all possible next board states with one specified symbol added from the current state.

#### `Board` instances
The library currently contains two `Board` instances.
The first is `SimpleBoard`. It is internally represented as a plain 3x3 2D list of `Symbols`.
Here is a showcase of some methods implemetations:
```haskell
-- simple 3x3 Board instance
newtype SimpleBoard = SimpleBoard [[Symbol]]

instance Board SimpleBoard where
  placeS (r, c) (SimpleBoard b) s =
    SimpleBoard (replaceAt r (replaceAt c s (b !! r)) b)

  printBoard (SimpleBoard b) = do putStrLn $ showBoard b

  empty = SimpleBoard $ replicate 3 $ replicate 3 E

  getS (r, c) (SimpleBoard b) = (b !! r) !! c

  getRow i (SimpleBoard b) = b !! i

  getCol i (SimpleBoard b) = map ( !! i ) b

```


Second instance is called `BigBoard`.\
Its It's meaning is a representation of arbitrarily infinite grid, for which some methods are a bit more complicated. 
It is located in module `src/Board/BigBoard.hs`.

---
### AI
The AI module handles responsibilities of the computer player.
- Evaluation functions are decoupled and abstracted, so they can be replaced or altered easily.
- The core algorithm used is a standard minimax. It takes an evaluation function as a parameter and chooses the best move up to a certain depth.
- For Tic-Tac-Toe, the entire game tree can be traversed.
- For larger boards, a static evaluation function is used.
- The AI module also provides a function to determine whether a board state is terminal


The most important function exported from the module `/src/AI/Minimax.hs` is `getBestMove`.
Its signature is:
```haskell
getBestMove :: (Board b) => (b -> Int) -> Int -> Int -> b -> Bool -> b
getBestMove                   eval       len     depth  b   maxi = nextBoard
```
Where the parameters correspond to:
- eval (b -> Int) - A static evaluation function. It takes in a Board instance and returns a single value
- len - The number of symbols that need to be in a line to win. (TicTacToe = 3, Connect5 = 5) 
- depth - The depth that the minimax recursion is allowed to reach.
- b - The current board. Since the board instance can generate all next boards, we don't need extra function parameter that generates successors
- maxi - A boolean representing both the symbol currently played, as well as the information, if we're currently the max, or min player
- nextBoard - The function will then return the next state.

The function now has all info to compute minimax value of all succeeding states.
The minimax function itself is very similar, with the main difference being it doesn't choose from states,
but it only computes current minimax value of the state. 

The getBestMove function expects to be called on non terminal node. In other words, the user is expected to check, if the board already has a winner, or if it is full, rendering a draw. This is done by a function `src/AI/Evaluator.isTerminal`.
```haskell
isTerminal :: (Board b) => Int -> b -> Bool
```
where the first parameter represents number of successive symbols needed for a player to win, and b is the current board. 



---

### App
Even though this project is primarily intended as a library for tictactoe based games, it does contain a small CLI application.
- The module contains functionality for playing the games via std IO.
- The CLI serves as a demonstration and proof of concept for the library components.

### Unit Testing

Unit tests cover the non-trivial parts of the project.
- Currently, tests focus on verifying the correctness of win-detection logic (checking whether any player has won the game).
- Tests are implemented using the HUnit package. Cabal will automatically download this dependency.
- Tests can be run using:

```
cabal test
```

### Possible extensions
As stated above, the code is written in an abstract way.
- A new board data structure (for example, if the user wanted to represent the board as a DOK sparse matrix) can be introduced by defining a new type and then providing an implementation of the Board typeclass for it. This involves supplying definitions for all the methods required by the typeclass.
- The minimax implementation is also designed to be easily extended or optimized. Potential enhancements include adding alpha–beta pruning as a simple and cheap efficiency boost or refining heuristics (such as depth weighting) to improve decision quality.

### Code Examples

Below are some minimal examples of how to use the library components.

#### Creating and printing a board
```haskell
import Board.Board
import Board.BigBoard
import AI.Minimax

main :: IO ()
main = do
  -- mode <- askGamemode

  -- _ <- playGame mode

  let winLength = 5
  let searchDepth = 4
  -- here, the type annotation is crucial, since Board has several different instances
  let b0 = empty :: BigBoard

  -- manual way to place a symbol on the board
  let b1 = placeS (2,1) b0 O

  -- define static evaluation function for minimax 
  let eval = staticEval winLength
  
  -- function, that calls minimax, and returns the new board  
  let b2 = getBestMove eval winLength searchDepth b1 True

  -- IO monadic function to pretty print any board
  printBoard b2
  
  return ()
```



