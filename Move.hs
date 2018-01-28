module Move(
	Move(..),
	MoveType(..),
	displayMove
) where

import Coord
import Piece

data MoveType = 
	NormalMove | 
	CaptureMove |

	PromotionMove PieceType |
	PawnCapture |
	PawnDoubleMove |
	EnpassantMove | 

	LongCastlingMove |
	ShortCastlingMove 
	deriving (Show, Eq)

data Move = Move {
	kind :: MoveType,
	from :: Coord, 
	to 	 :: Coord
} deriving (Show, Eq)

displayMove :: Move -> String
displayMove (Move NormalMove f t) = coordToFen f ++ "-" ++ coordToFen t
displayMove (Move CaptureMove f t) = coordToFen f ++ "x" ++ coordToFen t
displayMove (Move (PromotionMove p) f t) = coordToFen f ++ "-" ++ coordToFen t ++ pieceTypeToFen p
displayMove (Move PawnCapture f t) = coordToFen f ++ "x" ++ coordToFen t
displayMove (Move PawnDoubleMove f t) = coordToFen f ++ "-" ++ coordToFen t
displayMove (Move EnpassantMove f t) = coordToFen f ++ "x" ++ coordToFen t

displayMove (Move LongCastlingMove f t) = "0-0-0"
displayMove (Move ShortCastlingMove f t) = "0-0"
