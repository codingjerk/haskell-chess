module Evaluation(
	evaluate
) where

import Position
import Data.Array
import Piece

type Score = Int

class Eval a where
	eval :: a -> Score

instance Eval PieceType where
	eval Pawn	= 100
	eval Knight	= 300
	eval Bishop	= 300
	eval Rook	= 500
	eval Queen	= 900
	eval King 	= 110000

instance Eval Piece where
	eval (Piece White t) = eval t
	eval (Piece Black t) = -eval t

instance (Eval a) => Eval (Maybe a) where
	eval Nothing = 0
	eval (Just p) = eval p 

evaluate :: Position -> Score
evaluate pos = sum $ map eval $ elems (board pos)
