module Test where

import Piece
import Board

board = addPiece ('a',1) (Piece White Rook) emptyBoard

main = putStrLn $ displayBoard board