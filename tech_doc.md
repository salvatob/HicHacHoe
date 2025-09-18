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

The AI module handles the coomputer player responsibilities.
The core algorithm used is a standard minimax.
Evaluation functions are decoupled, abstracted and ready to be easily replaced or altered.

The actual App provides comfortable console interface for playing on the std IO.
