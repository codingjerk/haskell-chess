-- | Module Perft contains a testing only one testing function 'perft'.

module Perft(
    perft
) where

import Position
import Generator

-- | Perft is an alogithm that calculate nodes count for some position
-- on some depth. This function gets a depth, position and returns nodes count.
--
-- This function is very usefull for testing and debuging generator, 'Board' and 'Position'.
perft :: Int -> Position -> Int
perft depth pos 
    | depth <= 0 = 1
    | otherwise  = sum leafs where
        leafs = map (perft $ depth - 1) $ legalPositions
        legalPositions = filter isLegalPosition nextpositions
        nextpositions = map (flip makeMove pos) $ moves pos
