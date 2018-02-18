module Generator(
	generateLow
) where

import Move
import Position
import Piece
import Coord
import Data.Array
import Data.Maybe

--generate :: Coord -> Position -> [Move]

validx :: Char -> Bool
validx x = 'a' <= x && x <= 'h'

validy :: Int -> Bool
validy y = 1 <= y && y <= 8

validcapt :: Coord -> PieceColor -> Position -> Bool
validcapt coord color pos = (toSquare == Nothing) || (color /= pieceColor (fromJust toSquare)) where
	toSquare = board pos ! coord

validmove :: Coord -> PieceColor -> Position -> Bool
validmove coord@(x, y) color pos = validx x && validy y && validcapt coord color pos

addx :: Char -> Int -> Char
addx x y = toEnum (fromEnum x + y)

moveType :: Coord -> Position -> MoveType
moveType coord pos = if (board pos) ! coord == Nothing then NormalMove else CaptureMove

generateLow :: Piece -> Coord -> Position -> [Move]
generateLow (Piece c Knight) (x, y) pos = 
	[Move (moveType (nx dx, ny dy) pos) (x, y) (nx dx, ny dy) | dx <- [-2..2], dy <- [-2..2], (abs dx + abs dy == 3), validmove (nx dx, ny dy) c pos ] where
		nx d = addx x d
		ny d = y + d

generateLow (Piece c King) (x, y) pos = 
	[Move (moveType (nx dx, ny dy) pos) (x, y) (nx dx, ny dy) | dx <- [-1..1], dy <- [-1..1], (dx /= 0 || dy /= 0), validmove (nx dx, ny dy) c pos ] where
		nx d = addx x d
		ny d = y + d
