module Board(
	Position(..),
	Square(..),
	Board,
	emptyBoard,
	displayBoard,
	addPiece,
	removePiece,
	setSquare,
	newBoard
) where

import Piece

import Data.Ix
import Data.Array

type Position = (Char, Int)

data Square = Empty | Occupied Piece
	deriving (Show, Eq)

displaySquare :: Square -> String
displaySquare Empty = "|...|"
displaySquare (Occupied p) = "|" ++ displayPiece p ++ "|"

type Board = Array Position Square

xranges :: (Char, Char)
xranges = ('a', 'h')

yranges :: (Int, Int)
yranges = (1, 8)

boardRanges :: (Position, Position)
boardRanges = ( (fst xranges, fst yranges), (snd xranges, snd yranges) )

emptyBoard :: Board
emptyBoard = array boardRanges $ map (\x -> (x, Empty)) $ range boardRanges

addPiece :: Position -> Piece -> Board -> Board
addPiece pos piece board 
	| board ! pos == Empty = setSquare pos (Occupied piece) board
	| otherwise = error "Trying to add piece on non empty square"

removePiece :: Position -> Board -> Board
removePiece pos board
	| board ! pos == Empty = error "Trying to remove piece from empty square"
	| otherwise = setSquare pos Empty board

setSquare :: Position -> Square -> Board -> Board
setSquare pos square board = board // [(pos, square)]

displayBoard :: Board -> String
displayBoard b = concatMap displayLine $ reverse [fst yranges .. snd yranges] where
	displayLine y = concatMap displayCell [fst xranges .. snd xranges] ++ "\n" where
		displayCell x = displaySquare $ b ! (x, y)

newBoard :: Board
newBoard = array boardRanges $ map (\x -> (x, piece x)) $ range boardRanges where
	white = Occupied . Piece White
	black = Occupied . Piece Black
	setup = listArray xranges [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
	piece (x, 1) = white $ setup ! x
	piece (_, 2) = white Pawn
	piece (_, 7) = black Pawn
	piece (x, 8) = black $ setup ! x
	piece _ = Empty
