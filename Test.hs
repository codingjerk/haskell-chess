module Test (
    test
) where

import System.Environment
import Position
import Perft

fen :: String
fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

position :: Position
position = positionFromFen fen

test :: IO ()
test = do
    args <- getArgs
    putStrLn $ "Число узлов для глубины " ++ depth args ++ ":"
    print $ perft (read $ depth args) position where
        depth args = head args
