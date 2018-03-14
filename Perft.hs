module Perft(
	perft
) where

import Position
import Generator

perft :: Int -> Position -> Int
perft depth pos 
	| depth <= 1 = length (moves pos)
	| otherwise  = sum $ map (\move -> perft (depth - 1) (makeMove move pos)) (moves pos)
