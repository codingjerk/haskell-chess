module Test where

import Position
import Move

fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

position = positionFromFen fen

new_position = makeMove (Move PawnDoubleMove ('e',2) ('e',4)) position

new_position2 = makeMove (Move EnpassantMove ('d',7) ('e',3)) new_position

main = do 
	putStrLn $ displayPosition position ++ "\n\n"
	putStrLn $ displayPosition new_position ++ "\n\n"
	putStrLn $ displayPosition new_position2