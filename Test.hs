-- | Module Test created for testing all other modules, 
-- and will be main module, until development done.

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

-- | test function gets nothing, but doing IO-actions and
-- printing result of testig in terminal.
test :: IO ()
test = do
    args <- getArgs
    putStrLn $ "Число узлов для глубины " ++ depth args ++ ":"
    print $ perft (read $ depth args) position where
        depth args = head args
