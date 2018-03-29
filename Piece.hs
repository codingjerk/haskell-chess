-- | Module Piece contains basic types 'Piece', 'PieceColor' and 'PieceType'.
-- Also in this module defined some usefull function to representing pieces
-- and transform they.

module Piece(
    -- * Types
    Piece(..),
    PieceColor(..),
    PieceType(..),

    -- * Extracting functions
    pieceType,
    pieceColor,

    -- * Transformation functions
    setType,

    -- * Display
    displayPiece,
    pieceToFen,
    pieceFromFen,
    pieceTypeToFen
) where

import Data.Char

-- | PieceColor implements basic values of chess - colors White and Black.
data PieceColor = Black | White
    deriving (Show, Read, Eq, Ord)

-- | PieceType implements types of pieces, without setting color to them.
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Read, Eq, Enum, Ord)

-- | Type Piece unions color and type of piece in complete piece, that 
-- has color and type
data Piece = Piece PieceColor PieceType
    deriving (Show, Read, Eq, Ord)

-- | pieceType gets a Piece and returns it's type.
--
-- Example:
--
-- >>> pieceType (Piece Black Pawn)
-- Pawn
pieceType :: Piece -> PieceType
pieceType (Piece _ t) = t

-- | pieceColor gets a Piece and returns it's color.
--
-- Example:
--
-- >>> pieceColor (Piece White Queen)
-- White
pieceColor :: Piece -> PieceColor
pieceColor (Piece c _) = c

-- | This function gets a piece, type and returns a new piece
-- with same color, but new type.
--
-- Example:
--
-- >>> setType (Queen) (Piece White Pawn)
-- Piece White Queen
setType :: PieceType -> Piece -> Piece
setType t (Piece c _) = Piece c t

-- | This function gets a piece and returns it's string representation
-- for displaing on 'Board'.
displayPiece :: Piece -> String
displayPiece (Piece White p) = map (toUpper) $ show p
displayPiece (Piece Black p) = map (toLower) $ show p

-- | pieceToFen function gets a piece and returns it's string representation
-- in FEN-notation. It is just one char.
pieceToFen :: Piece -> String
pieceToFen (Piece White Pawn)   = "P"
pieceToFen (Piece White Knight) = "N"
pieceToFen (Piece White Bishop) = "B"
pieceToFen (Piece White Rook)   = "R"
pieceToFen (Piece White Queen)  = "Q"
pieceToFen (Piece White King)   = "K"

pieceToFen (Piece Black Pawn)   = "p"
pieceToFen (Piece Black Knight) = "n"
pieceToFen (Piece Black Bishop) = "b"
pieceToFen (Piece Black Rook)   = "r"
pieceToFen (Piece Black Queen)  = "q"
pieceToFen (Piece Black King)   = "k"

-- | pieceFromFen function gets a character and returns Piece, what represent it
-- in FEN-notation. 
-- 
-- Uppercase characters is 'White' pieces.
-- Lowercase characters is 'Black' pieces.
--
-- Also, that function may generate error, if character isn't FEN-piece.
pieceFromFen :: Char -> Piece
pieceFromFen 'P' = (Piece White Pawn)  
pieceFromFen 'N' = (Piece White Knight)
pieceFromFen 'B' = (Piece White Bishop)
pieceFromFen 'R' = (Piece White Rook)  
pieceFromFen 'Q' = (Piece White Queen) 
pieceFromFen 'K' = (Piece White King)  

pieceFromFen 'p' = (Piece Black Pawn)  
pieceFromFen 'n' = (Piece Black Knight)
pieceFromFen 'b' = (Piece Black Bishop)
pieceFromFen 'r' = (Piece Black Rook)  
pieceFromFen 'q' = (Piece Black Queen) 
pieceFromFen 'k' = (Piece Black King)  

pieceFromFen x = error ("Unknown char: " ++ [x])

-- | pieceTypeToFen just gets PieceType and returns uppercase character,
-- that implements it in FEN-notation.
pieceTypeToFen :: PieceType -> String
pieceTypeToFen p = pieceToFen (Piece White p)
