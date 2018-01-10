module Piece(
	Piece(..),
	PieceColor(..),
	PieceType(..),
	displayPiece
) where

data PieceColor = Black | White
	deriving (Show, Read, Eq, Ord)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
	deriving (Show, Read, Eq, Enum, Ord)

data Piece = Piece PieceColor PieceType
	deriving (Show, Read, Eq, Ord)

displayPiece :: Piece -> String
displayPiece (Piece White Pawn)   = "PAW"
displayPiece (Piece White Knight) = "KNI"
displayPiece (Piece White Bishop) = "BIS"
displayPiece (Piece White Rook)   = "ROO"
displayPiece (Piece White Queen)  = "QUE"
displayPiece (Piece White King)   = "KIN"

displayPiece (Piece Black Pawn)   = "paw"
displayPiece (Piece Black Knight) = "kni"
displayPiece (Piece Black Bishop) = "bis"
displayPiece (Piece Black Rook)   = "roo"
displayPiece (Piece Black Queen)  = "que"
displayPiece (Piece Black King)   = "kin"