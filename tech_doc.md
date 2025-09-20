# Technical Documentation

Project is separated into 3 parts:

### Board
The main purpose of the Board module is to encapsulate behaviour of the playing board.

It contains features such as: 
- printing the board to std
- executing moves (adding symbols to the board)
- generating all moves
- resizing the board if needed


Most of the apps architecture uses the class (Board)
That would allow for alternative implementations to be easily used.   

It encapsulates standard methods, while hiding the actual implementation details.

---
### AI
The AI module handles responsibilities of the computer player.
Evaluation functions are decoupled, abstracted and ready to be easily replaced or altered.


The core algorithm used is a standard minimax.
It takes an evaluation function as a parameter, and chooses the best move to a certain depth. (TictacToe is simple enough to traverse the whole game tree)
The static evaluation function is consequently different for each gamemode. 
Also contains a function, that determines, whether the board state is terminal.

---
### App
Even though this project is supposed to serve primarily as a library for tictactoe based games, it does contain a small CLI app.  
This module contains functionability for playing the games on the std IO.



### Unit testing
Parts of the project, that are not as trivial, are covered in some basic unit tests.
Specifically this means functions for checking if any player has won the game.
Unit tests are implemented using the HUnit package. It should be downloaded automatically by cabal.
They can be then run with `cabal test`


