module Generator(
	generateLow,
	generate
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

generateLow (Piece White Pawn) (x, y) pos = double ++ one ++ captureLeft ++ captureRight where 
	double = if ( validmove (x, y + 2) White pos && y == 2 && (board pos ! (x, y + 1) == Nothing) && (board pos ! (x, y + 2) == Nothing) ) 
		then [Move PawnDoubleMove (x, y) (x, y + 2)]
		else []
	one = if ( validmove (x, y + 1) White pos && (board pos ! (x, y + 1) == Nothing) )
		then [Move NormalMove (x, y) (x, y + 1)]
		else []
	captureLeft = if ( validmove (addx x (-1), y + 1) White pos && (board pos ! (addx x (-1), y + 1) /= Nothing) )
		then [Move PawnCapture (x, y) (addx x (-1), y + 1)]
		else []
	captureRight = if ( validmove (addx x 1, y + 1) White pos && (board pos ! (addx x 1, y + 1) /= Nothing) )
		then [Move PawnCapture (x, y) (addx x 1, y + 1)]
		else []

generateLow (Piece Black Pawn) (x, y) pos = double ++ one ++ captureLeft ++ captureRight where 
	double = if ( validmove (x, y - 2) Black pos && y == 7 && (board pos ! (x, y - 1) == Nothing) && (board pos ! (x, y - 2) == Nothing) ) 
		then [Move PawnDoubleMove (x, y) (x, y - 2)]
		else []
	one = if ( validmove (x, y - 1) Black pos && (board pos ! (x, y - 1) == Nothing) )
		then [Move NormalMove (x, y) (x, y - 1)]
		else []
	captureLeft = if ( validmove (addx x (-1), y - 1) Black pos && (board pos ! (addx x (-1), y - 1) /= Nothing) )
		then [Move PawnCapture (x, y) (addx x (-1), y - 1)]
		else []
	captureRight = if ( validmove (addx x 1, y - 1) Black pos && (board pos ! (addx x 1, y - 1) /= Nothing) )
		then [Move PawnCapture (x, y) (addx x 1, y - 1)]
		else []

generate :: Coord -> Position -> [Move]
generate coord pos
	| pieceColor piece == turn pos = generateLow piece coord pos 
	| otherwise 				   = [] where
		piece = fromJust $ board pos ! coord
