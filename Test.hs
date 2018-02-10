module Test where

import Position

fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

position = positionFromFen fen

e2e4_position = makeMoveLow ('e',2) ('e',4) position

d7d5_position = makeMoveLow ('d',7) ('d',5) e2e4_position

e4d5_position = makeMoveLow ('e',4) ('d',5) d7d5_position

kingMoves_position = makeMoveLow ('e',8) ('d',7) e4d5_position

main = do 
	putStrLn $ displayPosition position ++ "\n\n"
	putStrLn $ displayPosition e2e4_position ++ "\n\n"
	putStrLn $ displayPosition d7d5_position ++ "\n\n"
	putStrLn $ displayPosition e4d5_position++ "\n\n"
	putStrLn $ displayPosition kingMoves_position