module Piece(
    Piece(..),
    PieceColor(..),
    PieceType(..),
    displayPiece,
    pieceToFen,
    pieceFromFen,
    pieceTypeToFen,
    pieceType,
    pieceColor,
    setType
) where

import Data.Char

data PieceColor = Black | White
    deriving (Show, Read, Eq, Ord)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show, Read, Eq, Enum, Ord)

data Piece = Piece PieceColor PieceType
    deriving (Show, Read, Eq, Ord)

pieceType :: Piece -> PieceType
pieceType (Piece _ t) = t

pieceColor :: Piece -> PieceColor
pieceColor (Piece c _) = c

setType :: PieceType -> Piece -> Piece
setType t (Piece c _) = Piece c t

displayPiece :: Piece -> String
displayPiece (Piece White p) = map (toUpper) $ show p
displayPiece (Piece Black p) = map (toLower) $ show p

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

pieceTypeToFen :: PieceType -> String
pieceTypeToFen p = pieceToFen (Piece White p)
