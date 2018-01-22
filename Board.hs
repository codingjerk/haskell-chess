module Board(
	Coord(..),
	Square(..),
	Board,
	emptyBoard,
	displayBoard,
	addPiece,
	removePiece,
	setSquare,
	newBoard,
	boardToFen,
	boardFromFen
) where

import Piece

import Data.Ix
import Data.Array
import Data.List
import Data.Char

type Coord = (Char, Int)

data Square = Empty | Occupied Piece
	deriving (Show, Eq)

displaySquare :: Square -> String
displaySquare Empty = "|...|"
displaySquare (Occupied p) = "|" ++ displayPiece p ++ "|"

squareAsFen :: Square -> String
squareAsFen Empty = "."
squareAsFen (Occupied p) = pieceToFen p

type Board = Array Coord Square

xranges :: (Char, Char)
xranges = ('a', 'h')

yranges :: (Int, Int)
yranges = (1, 8)

boardRanges :: (Coord, Coord)
boardRanges = ( (fst xranges, fst yranges), (snd xranges, snd yranges) )

emptyBoard :: Board
emptyBoard = array boardRanges $ map (\x -> (x, Empty)) $ range boardRanges

addPiece :: Coord -> Piece -> Board -> Board
addPiece pos piece board 
	| board ! pos == Empty = setSquare pos (Occupied piece) board
	| otherwise = error "Trying to add piece on non empty square"

removePiece :: Coord -> Board -> Board
removePiece pos board
	| board ! pos == Empty = error "Trying to remove piece from empty square"
	| otherwise = setSquare pos Empty board

setSquare :: Coord -> Square -> Board -> Board
setSquare pos square board = board // [(pos, square)]

cutOn :: Int -> [a] -> [[a]]
cutOn n [] = []
cutOn n xs = (take n xs): cutOn n (drop n xs)

boardToFen :: Board -> String
boardToFen b = concat $ replaceDots $ group $ toString where 
	replaceDots = map (\a -> if (elem '.' a) then show (length a) else a)
	toString = (concatMap (\x -> fenLine x ++ "/") $ reverse [succ $ fst yranges .. snd yranges]) ++ fenLine (fst yranges) where
	fenLine y = concatMap fenCell [fst xranges .. snd xranges] where
		fenCell x = squareAsFen $ b ! (x, y)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn e xs = takeWhile (/=e) xs: splitOn e (drop 1 $ dropWhile (/=e) xs)

boardFromFen :: String -> Board
boardFromFen fen = fromList $ map (createLine) splitedFen where
	createLine [] = []
	createLine (x:xs)
		| isDigit x = replicate (digitToInt x) Empty ++ createLine xs
		| otherwise = Occupied (pieceFromFen x): createLine xs
	splitedFen = splitOn '/' fen
	fromList :: [[Square]] -> Board
	fromList sqs = array boardRanges $ map (\(x, y) -> ((x,y), cell x y)) $ range boardRanges where
		cell x y = sqs !! ((snd yranges) - y) !! (fromEnum x - fromEnum (fst xranges))

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
	