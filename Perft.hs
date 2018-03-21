module Perft(
	perft
) where

import Position
import Generator

perft :: Int -> Position -> Int
perft depth pos 
	| depth <= 0 = 1
	| otherwise  = sum leafs where
		leafs = map (perft $ depth - 1) $ legalPositions
		legalPositions = filter isLegalPosition nextpositions
		nextpositions = map (flip makeMove pos) $ moves pos
