-- | Module Position contains type 'Position' (more complex version or 'Board'),
-- and some usefull functions to work with positions.

module Position(
    -- * Types
    TurnColor,
    Castling(..),
    Position(..),

    -- * Making moves
    makeMove,

    -- * Display
    displayPosition,
    positionFromFen,
    positionToFen
) where

import Board
import Data.Array
import Data.Maybe
import Piece
import Coord
import Move
import Data.Char

-- | TurnColor is just synomim to PieceColor and it implement color of side, what will making move in next turn.
type TurnColor = PieceColor

colorFromFen :: String -> TurnColor
colorFromFen (c:[])
    | toUpper c == 'W' = White
    | toUpper c == 'B' = Black

colorToFen :: TurnColor -> String
colorToFen (White) = "w"
colorToFen (Black) = "b"

-- | Castling type implements castlint availability for all directions and all colors in position.
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

-- | Position type is more complex version of 'Board'. It contains board
-- and additional information - side to turn, castling availability, 
-- enpassant square and some information about moves count.
data Position = Position {
    -- | Pieces emplacement.
    board :: Board,
    -- | Active color.
    turn  :: TurnColor,
    -- | Castling availability.
    castling :: Castling,
    -- | If a pawn has just made a two-square move, 
    -- this is the position "behind" the pawn. 
    -- This is recorded regardless of whether there is a pawn in 
    -- position to make an en passant capture.
    enpassant :: Maybe Coord,
    -- | This is the number of halfmoves since the last pawn advance or capture. 
    -- This is used to determine if a draw can be claimed under the fifty-move rule.
    halfmoveClock :: Integer,
    -- | The number of the full move. 
    -- It starts at 1, and is incremented after Black's move.
    fullmoveNumber :: Integer
} deriving (Show)

enpassantFromFen :: String -> Maybe Coord
enpassantFromFen "-" = Nothing
enpassantFromFen str = Just $ coordFromFen str

enpassantToFen :: Maybe Coord -> String
enpassantToFen Nothing = "-"
enpassantToFen (Just coord) = coordToFen coord

-- | positionFromFen gets a fen-string and returns a position for 
-- this record.
--
-- This function may generate error if fen is not valid. 
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

-- | displayPosition gets a position and returns it's String implementation
-- in cute view.
displayPosition :: Position -> String
displayPosition (Position board turn castl enp clock moves) = 
    displayBoard board ++ "\n" ++
    "Turn: " ++ show turn ++ "\n" ++
    "Castling Possibility: " ++ displayCastring castl ++ "\n" ++
    "Enpassant Coordinates: " ++ show enp ++ "\n" ++
    "Halfmove clock: " ++ show clock ++ "\n" ++
    "Fullmove number: " ++ show moves

-- | This function gets a position and returns it's String implementation
-- in FEN-notation.
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

-- | This function gets move and position and returns new position,
-- which is obtained after making move.
makeMove :: Move -> Position -> Position
makeMove move pos = makeMoveNoChecks move pos
