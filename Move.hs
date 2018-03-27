-- | Move module contains base type 'Move' and 
-- function to convert it in humanity-string.

module Move(
    -- * Types
    Move(..),
    MoveType(..),

    -- * Display-functions
    displayMove
) where

import Coord
import Piece

-- | This type contains kinds of moves.
data MoveType = 
    -- | NormalMove implements a non-capture common move.
    NormalMove | 
    -- | CaptureMove implements a common caprute.
    CaptureMove |

    -- | PromotionMove is 'Pawn'-specific move, that contains a type of piece to promote in.
    PromotionMove PieceType |
    -- | PawnCapture is 'Pawn'-specific move, that implements a capture move of 'Pawn'.
    PawnCapture |
    -- | PawnDoubleMove is move that can make 'Pawn' from start position.
    PawnDoubleMove |
    -- | Enpassant is a special chess move, that also is a capture.
    EnpassantMove | 

    -- | Long Castling (to 'Queen' side) is special 'King' move, that also affects 'Rook' on a-file 
    LongCastlingMove |
    -- | Short Castling (to 'King' side) is special 'King' move, that also affects 'Rook' on h-file 
    ShortCastlingMove 
    deriving (Show, Eq)

-- | Move type contains from, to coordinates and a type of move.
-- 
-- So, this is example of Move value:
--
-- > Move NormalMove ('b',3) ('f',3)
data Move = Move {
    kind :: MoveType,
    from :: Coord, 
    to   :: Coord

} deriving (Show, Eq)

-- | displayMove function gets a move and returns it's string representation.
-- 
-- As example:
-- 
-- > displayMove (Move CaptureMove ('e',2) ('e',6)) == "e2xe6"
displayMove :: Move -> String
displayMove (Move NormalMove f t) = coordToFen f ++ "-" ++ coordToFen t
displayMove (Move CaptureMove f t) = coordToFen f ++ "x" ++ coordToFen t
displayMove (Move (PromotionMove p) f t) = coordToFen f ++ "-" ++ coordToFen t ++ pieceTypeToFen p
displayMove (Move PawnCapture f t) = coordToFen f ++ "x" ++ coordToFen t
displayMove (Move PawnDoubleMove f t) = coordToFen f ++ "-" ++ coordToFen t
displayMove (Move EnpassantMove f t) = coordToFen f ++ "x" ++ coordToFen t
displayMove (Move LongCastlingMove f t) = "0-0-0"
displayMove (Move ShortCastlingMove f t) = "0-0"
