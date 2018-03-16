--module Test where

import Position
import Perft
import Generator
import Move

fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"

position = positionFromFen fen

depth :: String
depth = "1"

main = do
	--putStrLn "Введите глубину perft'а:"
	--depth <- getLine
	putStrLn $ "Число узлов для глубины " ++ depth ++ ":"
	print $ perft (read depth) position 
