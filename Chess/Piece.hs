module Chess.Piece(
	Piece(..),
	PieceColor(..),
	PieceType(..)
) where

data PieceColor = Black | White
	deriving (Show, Read, Eq, Enum, Ord)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
	deriving (Show, Read, Eq, Enum, Ord)

data Piece = Piece PieceColor PieceType
	deriving (Show, Read, Eq, Ord)