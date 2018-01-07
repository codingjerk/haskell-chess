module Chess.Piece (
	Piece, 
	PieceType, 
	PieceColor,
	pieceToChar,
	charToPiece
) where

import Data.Char

data PieceColor = Black | White
	deriving (Show, Read, Eq, Enum, Ord)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
	deriving (Show, Read, Eq, Enum, Ord)

pieceTypeToChar :: PieceType -> Char
pieceTypeToChar Pawn   = 'P'
pieceTypeToChar Knight = 'N'
pieceTypeToChar Bishop = 'B'
pieceTypeToChar Rook   = 'R'
pieceTypeToChar Queen  = 'Q'
pieceTypeToChar King   = 'K'

charToPieceType :: Char -> PieceType
charToPieceType 'P' = Pawn  
charToPieceType 'N' = Knight
charToPieceType 'B' = Bishop
charToPieceType 'R' = Rook  
charToPieceType 'Q' = Queen 
charToPieceType 'K' = King

charToPieceType 'p' = Pawn  
charToPieceType 'n' = Knight
charToPieceType 'b' = Bishop
charToPieceType 'r' = Rook  
charToPieceType 'q' = Queen 
charToPieceType 'k' = King

charToPieceType c   = error $ "Unknown piece: " ++ [c]

data Piece = Piece PieceColor PieceType
	deriving (Show, Read, Eq, Ord)

pieceToChar :: Piece -> Char
pieceToChar (Piece White t) = toUpper $ pieceTypeToChar t
pieceToChar (Piece Black t) = toLower $ pieceTypeToChar t

charToPiece :: Char -> Piece
charToPiece c = Piece color $ charToPieceType $ toUpper c
	where color = if isUpper c then White else Black