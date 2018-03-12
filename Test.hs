module Test where

import Position
import Move
import Generator
import Evaluation

fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

position = positionFromFen fen

main = do 
	putStrLn $ displayPosition position
	putStrLn $ "Score: " ++ show (evaluate position)
	putStr $ "Moves (" ++ show (length allmoves) ++ "): "
	print $ map displayMove (allmoves) where
		allmoves = moves position
