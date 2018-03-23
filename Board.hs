-- | The Board module is responsible for 
-- providing squares, boards, 
-- transfering them to a string, 
-- as well functions to add and remove shapes.

module Board(
    -- * Types
    Square(..),
    Board(..),

    -- * Common Boards
    emptyBoard,
    initialBoard,

    -- * Board's base functions
    addPiece,
    removePiece,
    setSquare,

    -- * Board's representation functions
    displayBoard,
    boardToFen,
    boardFromFen,

    -- * Size of Board
    xranges,
    yranges
) where

import Piece
import Coord

import Data.Ix
import Data.Array
import Data.List
import Data.Char

-- | Square is a synonim to Maybe Piece.
-- 
-- It may adopt a two values: 
-- 
--     * Nothing for empty square.
-- 
--     * Just 'Piece' for piece on square.
-- 
-- > Just (Piece White King)
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

-- | Board is array of Squares
-- where index of this array is coordinate ('Coord').
type Board = Array Coord Square

-- | This value contains a bounds of 'Board' by x-axis.
-- 
-- by default it is equal to @(\'a\', \'h\')@
xranges :: (Char, Char)
xranges = ('a', 'h')

-- | This value contains a bounds of 'Board' by y-axis.
-- 
-- by default it is equal to @(1, 8)@
yranges :: (Int, Int)
yranges = (1, 8)

boardRanges :: (Coord, Coord)
boardRanges = ( (fst xranges, fst yranges), (snd xranges, snd yranges) )

-- | 'emptyBoard' represent a blank board with no 'Piece's.
-- 
-- In other case emptyBoard is array of empty 'Square's.
emptyBoard :: Board
emptyBoard = array boardRanges $ map (\x -> (x, Nothing)) $ range boardRanges

-- | 'addPiece' gets a 'Piece' to set, 'Coord'inates and a 'Board' to set on
-- and returns a new 'Board' with a new setted 'Piece'.
-- 
-- This function may generate error, if you try to add 'Piece' over
-- another 'Piece'.
addPiece :: Coord -> Piece -> Board -> Board
addPiece pos piece board 
    | board ! pos == Nothing = setSquare pos (Just piece) board
    | otherwise = error "Trying to add piece on non Nothing square"

-- | 'removePiece' gets a 'Coord'inates and a 'Board' to remove piece
-- and returns a new 'Board' without removed 'Piece'.
--
-- This function may generate error, if you try to remove 'Piece' from
-- empty 'Square'.
removePiece :: Coord -> Board -> Board
removePiece pos board
    | board ! pos == Nothing = error "Trying to remove piece from Nothing square"
    | otherwise = setSquare pos Nothing board

-- | 'setSquare' gets a 'Coord'inates, 'Square' and a 'Board' to change
-- and returns a new 'Board' with changed 'Square'.
--
-- This function didn't generate erorrs and may be used then we don't
-- care that contain prevous Square.
setSquare :: Coord -> Square -> Board -> Board
setSquare pos square board = board // [(pos, square)]

cutOn :: Int -> [a] -> [[a]]
cutOn n [] = []
cutOn n xs = (take n xs): cutOn n (drop n xs)

boardToString :: (Square -> String) -> String -> String -> Board -> String
boardToString f squareSep lineSep b = intercalate lineSep $ map lineToString $ reverse [fst yranges .. snd yranges] where
    lineToString y = intercalate squareSep $ map (\x -> f $ b ! (x, y)) $ [fst xranges .. snd xranges]

-- | This function gets a 'Board' and returns it's String implementation
-- in FEN-notation.
boardToFen :: Board -> String
boardToFen b = concat $ replaceDots $ group $ boardToString squareAsFen "" "/" b where
    replaceDots = map (\a -> if (elem '.' a) then show (length a) else a)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn e xs = takeWhile (/=e) xs: splitOn e (drop 1 $ dropWhile (/=e) xs)

-- | 'boardFromFen' gets a fen-string and returns a 'Board' for 
-- this record.
--
-- This function may generate error if fen is not valid. 
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

-- | 'displayBoard' gets a 'Board' and returns it's String implementation
-- in cute view.
displayBoard :: Board -> String
displayBoard b = boardToString displaySquare "" "\n" b ++ "\n"

-- | 'initialBoard' returns a board with standart starting position in chess.
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
