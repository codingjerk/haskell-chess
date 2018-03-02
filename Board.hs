module Board(
	Square(..),
	Board,
	emptyBoard,
	displayBoard,
	addPiece,
	removePiece,
	setSquare,
	initialBoard,
	boardToFen,
	boardFromFen,
	xranges,
	yranges
) where

import Piece
import Coord

import Data.Ix
import Data.Array
import Data.List
import Data.Char

type Square = Maybe Piece

squareWidth :: Int
squareWidth = 2

squareFill :: Char
squareFill = ' '

displaySquare :: Square -> String
displaySquare Nothing = "|" ++ replicate squareWidth squareFill ++ "|"
displaySquare (Just p) = "|" ++ take squareWidth (displayPiece p) ++ "|"

squareAsFen :: Square -> String
squareAsFen Nothing = "."
squareAsFen (Just p) = pieceToFen p

type Board = Array Coord Square

xranges :: (Char, Char)
xranges = ('a', 'h')

yranges :: (Int, Int)
yranges = (1, 8)

boardRanges :: (Coord, Coord)
boardRanges = ( (fst xranges, fst yranges), (snd xranges, snd yranges) )

emptyBoard :: Board
emptyBoard = array boardRanges $ map (\x -> (x, Nothing)) $ range boardRanges

addPiece :: Coord -> Piece -> Board -> Board
addPiece pos piece board 
	| board ! pos == Nothing = setSquare pos (Just piece) board
	| otherwise = error "Trying to add piece on non Nothing square"

removePiece :: Coord -> Board -> Board
removePiece pos board
	| board ! pos == Nothing = error "Trying to remove piece from Nothing square"
	| otherwise = setSquare pos Nothing board

setSquare :: Coord -> Square -> Board -> Board
setSquare pos square board = board // [(pos, square)]

cutOn :: Int -> [a] -> [[a]]
cutOn n [] = []
cutOn n xs = (take n xs): cutOn n (drop n xs)

boardToString :: (Square -> String) -> String -> String -> Board -> String
boardToString f squareSep lineSep b = intercalate lineSep $ map lineToString $ reverse [fst yranges .. snd yranges] where
    lineToString y = intercalate squareSep $ map (\x -> f $ b ! (x, y)) $ [fst xranges .. snd xranges]

boardToFen :: Board -> String
boardToFen b = concat $ replaceDots $ group $ boardToString squareAsFen "" "/" b where
	replaceDots = map (\a -> if (elem '.' a) then show (length a) else a)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn e xs = takeWhile (/=e) xs: splitOn e (drop 1 $ dropWhile (/=e) xs)

boardFromFen :: String -> Board
boardFromFen fen = fromList $ map (createLine) splitedFen where
	createLine [] = []
	createLine (x:xs)
		| isDigit x = replicate (digitToInt x) Nothing ++ createLine xs
		| otherwise = Just (pieceFromFen x): createLine xs
	splitedFen = splitOn '/' fen
	fromList :: [[Square]] -> Board
	fromList sqs = array boardRanges cells where
		cells = [ (i, sq) | (i, sq) <- indexed] where
			indexed 
				| length transposedsqs /= length indexes = error "Fen notation represent a board with different size"
				| otherwise = zip indexes $ transposedsqs where
					transposedsqs = concat $ map reverse $ transpose sqs
					indexes = range boardRanges

displayBoard :: Board -> String
displayBoard b = boardToString displaySquare "" "\n" b ++ "\n"

initialBoard :: Board
initialBoard = array boardRanges $ map (\x -> (x, piece x)) $ range boardRanges where
	white = Just . Piece White
	black = Just . Piece Black
	setup = listArray xranges [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
	piece (x, 1) = white $ setup ! x
	piece (_, 2) = white Pawn
	piece (_, 7) = black Pawn
	piece (x, 8) = black $ setup ! x
	piece _ = Nothing
