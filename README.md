# HicHacHoe
## A simple tictactoe program written in the Haskell programming language
#### Project for the class NPRG005 of MFF UK

---

### What the program does

This program lets you play Tic-Tac-Toe (and its larger-board variants) against the computer in your terminal window. The computer uses an AI algorithm to make its moves, so you can test your skill against it.

### How to start the program
Whole project is managed with the CABAL tool.
For a simple run you:
1. Download this repository
2. Navigate to the root folder where the program is located.
3. Run the program using:
```
cabal run
```
(or the equivalent command if you compiled it differently).\
The game will start immediately in interactive mode.

### How to play
User can choose between two games. Simple 3x3 __Tic-Tac-Toe__, or __connect 5__ (also called gomoku) on an infinite grid.

- The program will display the game board in text form.
- Upon choosing a game mode and opponent, you will be prompted to enter your move.
- Moves are entered as coordinates, e.g. e1 means “row 1, column 5.”
- After your move, the computer will respond with its own move.
- The game continues until either you or the computer wins, or the board is full.
- Use `Ctrl+C` at any time to exit.

### Example session
```
Which game do you want to play? ('1'/'2')
1. standard TicTacToe
2. larger version - connect X
>> 1
Who do you want to play against? ('1'/'2')
1. another human player
2. AI player
>> 2
------
  a b c
1 .|.|.
2 .|.|.
3 .|.|.

Please input your next O move: example: b1
>> c1
  a b c
1 .|.|O
2 .|.|.
3 .|.|.

Computer playing...
  a b c
1 .|.|O
2 .|X|.
3 .|.|.

Please input your next O move: example: b1
>> a2
  a b c
1 .|.|O
2 O|X|.
3 .|.|.
```
May users be warned, that the app currently doesn't support move validation, so behaviour after any illegal move is undefined. 



