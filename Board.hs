module Board(
	Position,
	Square(..),
	Board(..),
	emptyBoard,
	displayBoard,
	addPiece
) where

import Piece

import Data.Ix
import Data.Array
import Data.Foldable

type Position = (Char, Int)

data Square = Empty | Occupied Piece
	deriving (Show)

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
addPiece pos piece board = board // [(pos, Occupied piece)]

displayBoard :: Board -> String
displayBoard b = displayBoard2 8 where
	displayBoard2 1 = displayLine 1 b
	displayBoard2 l = displayLine l b ++ "\n" ++ displayBoard2 (pred l)

displayLine :: Int -> Board -> String
displayLine i b = displayLine2 'a' where
	displayLine2 'h' = displayCell 'h' i b
	displayLine2 c = displayCell c i b ++ displayLine2 (succ c)

displayCell :: Char -> Int -> Board -> String
displayCell x y b = displaySquare $ b ! (x, y)
