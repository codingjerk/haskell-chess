module Test where

import Piece
import Board

board = 
      addPiece ('a',1) (Piece White Rook) 
	$ addPiece ('b',1) (Piece White Knight) 
	$ addPiece ('c',1) (Piece White Bishop) 
	$ addPiece ('d',1) (Piece White Queen) 
	$ addPiece ('e',1) (Piece White King) 
	$ addPiece ('f',1) (Piece White Bishop)
	$ addPiece ('g',1) (Piece White Knight) 
	$ addPiece ('h',1) (Piece White Rook) 
	emptyBoard

main = putStr $ displayBoard board
