module Test where

import Piece
import Board
import Position

position = positionFromFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

main = putStrLn $ displayPosition position
