-- | Evaluation module contains functions and types, that implements
-- position scoring and evaluating.

module Evaluation(
    -- * Types
    Score(..),

    -- * Main part
    infinity,
    evaluate
) where

import Position
import Data.Array
import Piece

-- | Type 'Score' implemets objective value of 'Piece' or 'Position'
-- as example.
type Score = Int

infinity = eval King

class Eval a where
    eval :: a -> Score

instance Eval PieceType where
    eval Pawn   = 100
    eval Knight = 300
    eval Bishop = 300
    eval Rook   = 500
    eval Queen  = 900
    eval King   = 110000

instance Eval Piece where
    eval (Piece White t) = eval t
    eval (Piece Black t) = -eval t

instance (Eval a) => Eval (Maybe a) where
    eval Nothing = 0
    eval (Just p) = eval p 

-- | Function 'evaluate' gets a position and returns it's score
evaluate :: Position -> Score
evaluate pos = sum $ map eval $ elems (board pos)
