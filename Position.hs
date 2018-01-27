module Position(
	TurnColor,
	Castling(..),
	Position(..),
	positionFromFen,
	displayPosition,
	positionToFen
) where

import Board
import Piece
import Coord
import Data.Char

type TurnColor = PieceColor

colorFromFen :: String -> TurnColor
colorFromFen (c:[])
	| toUpper c == 'W' = White
	| toUpper c == 'B' = Black

colorToFen :: TurnColor -> String
colorToFen (White) = "w"
colorToFen (Black) = "b"

data Castling = Castling {
	whiteLong :: Bool,
	whiteShort :: Bool,
	blackLong :: Bool,
	blackShort :: Bool
} deriving (Show)

displayCastring :: Castling -> String
displayCastring c = wl ++ ws ++ bl ++ bs where
	wl = if whiteLong c then  "White can long castling. "  else ""
	ws = if whiteShort c then "White can short castling. " else ""
	bl = if blackLong c then  "Black can long castling. "  else ""
	bs = if blackShort c then "Black can short castling. " else ""

castlingToFen :: Castling -> String
castlingToFen c = ws ++ wl ++ bs ++ bl where
	wl = if whiteLong c then  "Q"  else ""
	ws = if whiteShort c then "K" else ""
	bl = if blackLong c then  "q"  else ""
	bs = if blackShort c then "k" else ""

castlingFromFen :: String -> Castling
castlingFromFen [] = Castling False False False False
castlingFromFen "-" = Castling False False False False
castlingFromFen ('K':xs) = (castlingFromFen xs) {whiteShort = True}
castlingFromFen ('k':xs) = (castlingFromFen xs) {blackShort = True}
castlingFromFen ('Q':xs) = (castlingFromFen xs) {whiteLong = True}
castlingFromFen ('q':xs) = (castlingFromFen xs) {blackLong = True}

data Position = Position {
	board :: Board,
	turn  :: TurnColor,
	castling :: Castling,
	enpassant :: Maybe Coord,
	halfmoveClock :: Integer,
	fullmoveNumber :: Integer
} deriving (Show)

enpassantFromFen :: String -> Maybe Coord
enpassantFromFen "-" = Nothing
enpassantFromFen str = Just $ coordFromFen str

enpassantToFen :: Maybe Coord -> String
enpassantToFen Nothing = "-"
enpassantToFen (Just coord) = coordToFen coord

positionFromFen :: String -> Position
positionFromFen str = makePosition $ words str where
	makePosition (pieces: turn: castling: enpassant: clock: movesNumber: []) = 
		Position 
			(boardFromFen pieces)
			(colorFromFen turn)
			(castlingFromFen castling)
			(enpassantFromFen enpassant)
			(read clock :: Integer)
			(read movesNumber :: Integer) 

displayPosition :: Position -> String
displayPosition (Position board turn castl enp clock moves) = 
	displayBoard board ++ "\n\n" ++
	"Turn: " ++ show turn ++ "\n" ++
	"Castling Possibility: " ++ displayCastring castl ++ "\n\n" ++
	"Enpassant Coordinates: " ++ show enp ++ "\n\n" ++
	"Halfmove clock: " ++ show clock ++ "\n\n" ++
	"Fullmove number: " ++ show moves

positionToFen :: Position -> String
positionToFen (Position board turn castl enp clock moves) =
	boardToFen board ++ " " ++
	colorToFen turn ++ " " ++
	castlingToFen castl ++ " " ++
	enpassantToFen enp ++ " " ++
	show clock ++ " " ++
	show moves