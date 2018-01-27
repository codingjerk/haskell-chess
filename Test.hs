module Test where

import Position

fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

position = positionFromFen fen

main = do 
	putStrLn $ displayPosition position
	putStrLn "\n---\n"

	putStrLn $ positionToFen position
	putStrLn "\n---\n"

	print $ fen == positionToFen position