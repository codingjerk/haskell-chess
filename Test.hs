module Test where

import Position

fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

position = positionFromFen fen

e2e4_position = makeMoveLow ('e',2) ('e',4) position

main = do 
	putStrLn $ displayPosition position ++ "\n\n"
	putStrLn $ displayPosition e2e4_position