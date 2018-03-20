--module Test where

import System.Environment
import Position
import Perft
import Move
import Generator

fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

position = positionFromFen fen

main = do
	args <- getArgs
	putStrLn $ "Число узлов для глубины " ++ depth args ++ ":"
	print $ perft (read $ depth args) position where
		depth args = head args
