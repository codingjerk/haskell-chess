module Generator(
	generateLow,
	generate,
	moves
) where

import Move
import Position
import Piece
import Coord
import Data.Array
import Data.Maybe

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

generateLow (Piece White Pawn) (x, y) pos = proms ++ captureLeft ++ captureRight ++ double ++ one where 
	double = if ( validmove (x, y + 2) White pos && y == 2 && (board pos ! (x, y + 1) == Nothing) && (board pos ! (x, y + 2) == Nothing) ) 
		then [Move PawnDoubleMove (x, y) (x, y + 2)]
		else []
	one = if ( y /= 7 && validmove (x, y + 1) White pos && (board pos ! (x, y + 1) == Nothing) )
		then [Move NormalMove (x, y) (x, y + 1)]
		else []
	captureLeft = if ( y /= 7 && validmove (addx x (-1), y + 1) White pos && (board pos ! (addx x (-1), y + 1) /= Nothing) )
		then [Move PawnCapture (x, y) (addx x (-1), y + 1)]
		else []
	captureRight = if ( y /= 7 && validmove (addx x 1, y + 1) White pos && (board pos ! (addx x 1, y + 1) /= Nothing) )
		then [Move PawnCapture (x, y) (addx x 1, y + 1)]
		else []
	proms = if ( y == 7 )
		then filter (\(Move _ _ to) -> validmove to White pos) $ [Move (PromotionMove p) (x, y) (addx x dx, 8) | dx <- [-1, 1], moveType (addx x dx, 8) pos == CaptureMove, p <- [Queen, Rook, Bishop, Knight]]
			++ [Move (PromotionMove p) (x, y) (x, 8) | p <- [Queen, Rook, Bishop, Knight]]
		else []

generateLow (Piece Black Pawn) (x, y) pos = proms ++ captureLeft ++ captureRight ++ double ++ one where 
	double = if ( validmove (x, y - 2) Black pos && y == 7 && (board pos ! (x, y - 1) == Nothing) && (board pos ! (x, y - 2) == Nothing) ) 
		then [Move PawnDoubleMove (x, y) (x, y - 2)]
		else []
	one = if ( y /= 2 && validmove (x, y - 1) Black pos && (board pos ! (x, y - 1) == Nothing) )
		then [Move NormalMove (x, y) (x, y - 1)]
		else []
	captureLeft = if ( y /= 2 && validmove (addx x (-1), y - 1) Black pos && (board pos ! (addx x (-1), y - 1) /= Nothing) )
		then [Move PawnCapture (x, y) (addx x (-1), y - 1)]
		else []
	captureRight = if ( y /= 2 && validmove (addx x 1, y - 1) Black pos && (board pos ! (addx x 1, y - 1) /= Nothing) )
		then [Move PawnCapture (x, y) (addx x 1, y - 1)]
		else []
	proms = if ( y == 2 )
		then filter (\(Move _ _ to) -> validmove to Black pos) $ [Move (PromotionMove p) (x, y) (addx x dx, 1) | dx <- [-1, 1], moveType (addx x dx, 1) pos == CaptureMove, p <- [Queen, Rook, Bishop, Knight]]
			++ [Move (PromotionMove p) (x, y) (x, 1) | p <- [Queen, Rook, Bishop, Knight]]
		else []

generateLow (Piece color Rook) coord@(x, y) pos = rookcaptures ++ up ++ down ++ left ++ right where
	up    = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (x, y+dy) pos) (x, y) (x, y + dy) | dy <- [1..8] ]
	down  = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (x, y-dy) pos) (x, y) (x, y - dy) | dy <- [1..8] ]
	left  = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x (-dx), y) pos) (x, y) (addx x (-dx), y) | dx <- [1..8] ]
	right = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x dx, y) pos) (x, y) (addx x dx, y) | dx <- [1..8] ]
	rookcaptures = upcap ++ downcap ++ leftcap ++ rightcap where
		upcap    = first (\(Move t _ _) -> t == CaptureMove ) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (x, y+dy) pos) (x, y) (x, y + dy) | dy <- [1..8] ]
		downcap  = first (\(Move t _ _) -> t == CaptureMove ) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (x, y-dy) pos) (x, y) (x, y - dy) | dy <- [1..8] ]
		leftcap  = first (\(Move t _ _) -> t == CaptureMove ) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x (-dx), y) pos) (x, y) (addx x (-dx), y) | dx <- [1..8] ]
		rightcap = first (\(Move t _ _) -> t == CaptureMove ) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x dx, y) pos) (x, y) (addx x dx, y) | dx <- [1..8] ]
	first f xs = take 1 $ filter f xs

generateLow (Piece color Bishop) coord@(x, y) pos = bishopcaptures ++ upleft ++ upright ++ downleft ++ downright where
	upleft    = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x (-d), y+d) pos) (x, y) (addx x (-d), y + d) | d <- [1..8] ]
	upright   = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x d, y+d) pos) (x, y) (addx x d, y + d) | d <- [1..8] ]
	downleft  = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x (-d), y-d) pos) (x, y) (addx x (-d), y - d) | d <- [1..8] ]
	downright = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x d, y-d) pos) (x, y) (addx x d, y - d) | d <- [1..8] ]
	bishopcaptures = upleftcap ++ uprightcap ++ downleftcap ++ downrightcap where
		upleftcap    = first (\(Move t _ _) -> t == CaptureMove) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x (-d), y+d) pos) (x, y) (addx x (-d), y + d) | d <- [1..8] ]
		uprightcap   = first (\(Move t _ _) -> t == CaptureMove) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x d, y+d) pos) (x, y) (addx x d, y + d) | d <- [1..8] ]
		downleftcap  = first (\(Move t _ _) -> t == CaptureMove) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x (-d), y-d) pos) (x, y) (addx x (-d), y - d) | d <- [1..8] ]
		downrightcap = first (\(Move t _ _) -> t == CaptureMove) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x d, y-d) pos) (x, y) (addx x d, y - d) | d <- [1..8] ]
	first f xs = take 1 $ filter f xs

generateLow (Piece color Queen) coord@(x, y) pos = rookcaptures ++ bishopcaptures ++ up ++ down ++ left ++ right ++ upleft ++ upright ++ downleft ++ downright where
	up    = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (x, y+dy) pos) (x, y) (x, y + dy) | dy <- [1..8] ]
	down  = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (x, y-dy) pos) (x, y) (x, y - dy) | dy <- [1..8] ]
	left  = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x (-dx), y) pos) (x, y) (addx x (-dx), y) | dx <- [1..8] ]
	right = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x dx, y) pos) (x, y) (addx x dx, y) | dx <- [1..8] ]
	rookcaptures = upcap ++ downcap ++ leftcap ++ rightcap where
		upcap    = first (\(Move t _ _) -> t == CaptureMove ) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (x, y+dy) pos) (x, y) (x, y + dy) | dy <- [1..8] ]
		downcap  = first (\(Move t _ _) -> t == CaptureMove ) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (x, y-dy) pos) (x, y) (x, y - dy) | dy <- [1..8] ]
		leftcap  = first (\(Move t _ _) -> t == CaptureMove ) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x (-dx), y) pos) (x, y) (addx x (-dx), y) | dx <- [1..8] ]
		rightcap = first (\(Move t _ _) -> t == CaptureMove ) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x dx, y) pos) (x, y) (addx x dx, y) | dx <- [1..8] ]
	upleft    = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x (-d), y+d) pos) (x, y) (addx x (-d), y + d) | d <- [1..8] ]
	upright   = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x d, y+d) pos) (x, y) (addx x d, y + d) | d <- [1..8] ]
	downleft  = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x (-d), y-d) pos) (x, y) (addx x (-d), y - d) | d <- [1..8] ]
	downright = takeWhile (\(Move t _ to) -> validmove to color pos && t /= CaptureMove) [ Move (moveType (addx x d, y-d) pos) (x, y) (addx x d, y - d) | d <- [1..8] ]
	bishopcaptures = upleftcap ++ uprightcap ++ downleftcap ++ downrightcap where
		upleftcap    = first (\(Move t _ _) -> t == CaptureMove) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x (-d), y+d) pos) (x, y) (addx x (-d), y + d) | d <- [1..8] ]
		uprightcap   = first (\(Move t _ _) -> t == CaptureMove) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x d, y+d) pos) (x, y) (addx x d, y + d) | d <- [1..8] ]
		downleftcap  = first (\(Move t _ _) -> t == CaptureMove) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x (-d), y-d) pos) (x, y) (addx x (-d), y - d) | d <- [1..8] ]
		downrightcap = first (\(Move t _ _) -> t == CaptureMove) $ takeWhile (\(Move t _ to) -> validmove to color pos) [ Move (moveType (addx x d, y-d) pos) (x, y) (addx x d, y - d) | d <- [1..8] ]
	first f xs = take 1 $ filter f xs

generate :: Coord -> Position -> [Move]
generate coord pos
	| square == Nothing            = []
	| pieceColor piece /= turn pos = []
	| otherwise 				   = generateLow piece coord pos where
		square = board pos ! coord
		piece = fromJust square

moves :: Position -> [Move]
moves pos = concat $ map (\coord -> generate coord pos) ixs where
	ixs = indices (board pos)
