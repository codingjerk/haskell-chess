module Search (
    minimax,
    search
) where

import Position
import Evaluation
import Generator
import Move

minimax :: Int -> Position -> Score
minimax depth pos 
    | depth <= 0 = evaluate pos
    | otherwise  = maximum $ map (negate . (minimax $ depth - 1)) allpos where
        allpos = map (flip makeMove pos) $ moves pos

search :: Int -> Position -> Move
search depth pos = snd findbestmove where
    findbestmove = foldr (\x acc -> if fst x > fst acc then x else acc) ((-infinity), nullmove) allmoves
    allmoves = zip (scores allpos) (moves pos)
    allpos   = map (flip makeMove pos) $ moves pos
    scores   = map (negate . (minimax $ depth - 1))
    nullmove = Move NullMove ('a',1) ('a',1)
