tic-tac-clj
===========

My first clojure learning project. A simple client/server tic-tac-toe game.

Instructions:
=============
$ lein run
Runs simple server on port 7623 (port number can be specified on command line)

then simply connect to that server as follows:
$ telnet localhost 7623

After connection is made, there are a few commands:
new : creates a new tic-tac-toe game
info: displays the current game board
move: a move is specified as follows: move [x y] where x and y are the x and y coordinates of the 3x3 board
      so if i want to make a move to the center, i would need to issue the command : move [1 1] etc..
quit: doesn't do anything at the moment (to be fixed). You can always quit by issueing a command-c on server.
help: displays help.

The order of who goes first is determined at random.
