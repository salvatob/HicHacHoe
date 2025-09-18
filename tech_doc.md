Project is separated into 3 parts:
- App
- Board
- AI

The main purpose of the Board module is to encapsulate behaviour of the playing board.
It contains features such as: 
- printing the board to std
- executing moves (adding symbols to the board)
- generating all moves
- resizing the board if needed

This project was written as a library, focusing on abstraction, rather than being a standalone usable module. 
Most of the apps architecture uses the class (Board)
It encapsulates standard methods, while hiding the actual implementation details.
That would allow for alternative implementations to be easily used.   

---
The AI module handles the coomputer player responsibilities.
The core algorithm used is a standard minimax.
Evaluation functions are decoupled, abstracted and ready to be easily replaced or altered.

---

The actual App provides comfortable console interface for playing on the std IO.


Unit tests are implemented using the HUnit package. It should be downloaded automatically by cabal.
They can be then run with `cabal test`

