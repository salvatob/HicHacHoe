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

---
### AI
The AI module handles responsibilities of the computer player.
- Evaluation functions are decoupled and abstracted, so they can be replaced or altered easily.
- The core algorithm used is a standard minimax. It takes an evaluation function as a parameter and chooses the best move up to a certain depth.
- For Tic-Tac-Toe, the entire game tree can be traversed.
- For larger boards, a static evaluation function os used.
- The AI module also provides a function to determine whether a board state is terminal


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
