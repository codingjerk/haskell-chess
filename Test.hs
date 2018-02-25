module Test where

import Position
import Move
import Generator

fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

position = positionFromFen fen

main = do 
	putStrLn $ displayPosition position ++ "\n"

	print $ map (displayMove) $ moves position
