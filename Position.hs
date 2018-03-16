module Position(
	TurnColor,
	Castling(..),
	Position(..),
	positionFromFen,
	displayPosition,
	positionToFen,
	makeMove,
	isLegalPosition
) where

import Board
import Data.Array
import Data.Maybe
import Piece
import Coord
import Move
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
	wl = if whiteLong  c then "White can long castling. "  else ""
	ws = if whiteShort c then "White can short castling. " else ""
	bl = if blackLong  c then "Black can long castling. "  else ""
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
	displayBoard board ++ "\n" ++
	"Turn: " ++ show turn ++ "\n" ++
	"Castling Possibility: " ++ displayCastring castl ++ "\n" ++
	"Enpassant Coordinates: " ++ show enp ++ "\n" ++
	"Halfmove clock: " ++ show clock ++ "\n" ++
	"Fullmove number: " ++ show moves

positionToFen :: Position -> String
positionToFen (Position board turn castl enp clock moves) =
	boardToFen board ++ " " ++
	colorToFen turn ++ " " ++
	castlingToFen castl ++ " " ++
	enpassantToFen enp ++ " " ++
	show clock ++ " " ++
	show moves

data Triple = TTrue | TFalse | TMaybe
	deriving Eq

makeMoveLow :: Coord -> Coord -> Triple -> Position -> Position
makeMoveLow from to isCaptureMove pos@(Position board turn castl enp clock moves) = 
	pos { 
		board = nextboard, 
		turn = nextturn, 
		fullmoveNumber = nextmoves, 
		halfmoveClock = nexthalf,
		castling = nextcastling piece,
		enpassant = Nothing
	} where
		nextboard = removePiece from $ addPiece to piece boardWithEmptyToSquare where
			boardWithEmptyToSquare = if (isCaptureMove == TTrue) || (isCaptureMove == TMaybe && isRealCaptureMove) then removePiece to board else board
		piece = fromJust $ board ! from
		nextturn = if turn == White then Black else White
		nextmoves = if turn == Black then (moves + 1) else moves
		nexthalf = if (isRealCaptureMove || (pieceType piece == Pawn)) then 0 else (clock + 1)
		isRealCaptureMove = (board ! to) /= Nothing
		nextcastling (Piece White King) = castl {whiteLong = False, whiteShort = False}
		nextcastling (Piece Black King) = castl {blackLong = False, blackShort = False}
		nextcastling (Piece White Rook) = if fst from == fst xranges 
			then castl {whiteLong = False}
			else castl {whiteShort = False}
		nextcastling (Piece Black Rook) = if fst from == fst xranges 
			then castl {blackLong = False}
			else castl {blackShort = False}
		nextcastling p = castl

makeMoveNoChecks :: Move -> Position -> Position
makeMoveNoChecks (Move NormalMove f t) pos = (makeMoveLow f t TFalse pos)
makeMoveNoChecks (Move CaptureMove f t) pos = (makeMoveLow f t TTrue pos)

makeMoveNoChecks (Move (PromotionMove p) f t) pos = nextpos {board = nextboard} where
	nextpos = (makeMoveLow f t TMaybe pos)
	nextboard = setSquare t (Just nextpiece) (board nextpos) where
		nextpiece = setType p (fromJust $ board pos ! f)

makeMoveNoChecks (Move PawnCapture f t) pos = (makeMoveLow f t TTrue pos)
makeMoveNoChecks (Move PawnDoubleMove f t) pos = (makeMoveLow f t TFalse pos) {enpassant = nextenpass} where
	nextenpass = Just $ averageCoord f t where
		averageCoord f t = (fst f, div (snd t + snd f) 2)

makeMoveNoChecks (Move EnpassantMove f t) pos = nextpos {board = nextboard} where
	nextpos = (makeMoveLow f t TFalse pos)
	nextboard = removePiece (fst coord, sndcoord) (board nextpos) where
		coord = (fromJust $ enpassant pos)
		sndcoord = if snd coord == 3 then 4 else 5

makeMoveNoChecks (Move LongCastlingMove f t) pos = makeMoveNoChecks (Move NormalMove rookFrom rookTo) (makeMoveLow f t TFalse pos) where
	rookFrom = ('a', snd f)
	rookTo = ('d', snd f)

makeMoveNoChecks (Move ShortCastlingMove f t) pos = makeMoveNoChecks (Move NormalMove rookFrom rookTo) (makeMoveLow f t TFalse pos) where
	rookFrom = ('h', snd f)
	rookTo = ('f', snd f)

-- TODO: Create move validation function for testing
isValidMove :: Move -> Position -> Bool
isValidMove move pos = True

makeMove :: Move -> Position -> Position
makeMove move pos 
	| isValidMove move pos = makeMoveNoChecks move pos
	| otherwise 	       = error "Internal error: invalid move" 

isAttacked :: Coord -> Position -> Bool
isAttacked coord pos = undefined

findPiece :: Piece -> Position -> [Coord]
findPiece piece (Position board _ _ _ _ _) = map (fst) $ filter (\x -> snd x == Just piece) (assocs board)

invertColor :: TurnColor -> TurnColor
invertColor Black = White
invertColor White = Black

isLegalPosition :: Position -> Bool
isLegalPosition pos = not $ (null kings) || (isAttacked kingCoord pos) where
	kings = findPiece (Piece color King) pos
	kingCoord = head $ kings
	color = invertColor (turn pos)
