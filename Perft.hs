module Perft(
	perft
) where

import Position
import Generator

perft :: Int -> Position -> Int
perft depth pos 
	| depth <= 0 = 1
	| otherwise  = sum $ map (\nextpos -> perft (depth - 1) nextpos) $ legalPositions where
		legalPositions = filter isLegalPosition nextpositions
		nextpositions = map (\move -> makeMove move pos) $ (moves pos)
