module Coord(
	Coord(..),
	coordFromFen,
	coordToFen
) where

import Data.Char

type Coord = (Char, Int)

coordFromFen :: String -> Coord
coordFromFen (x:y:[]) = (x, digitToInt y)

coordToFen :: Coord -> String
coordToFen (x, y) = x: (show y)