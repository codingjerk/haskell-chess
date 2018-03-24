-- | Coord module contains basics definitions of Coord type 
-- and some operations on it.

module Coord(
    -- * Types
    Coord(..),
    -- * Representation
    coordFromFen,
    coordToFen
) where

import Data.Char

-- | This type implements chess coordinates as tuple of Char and Int.
-- 
-- As example, coordinates of square e4 will be looks like this:
--
-- > ('e', 4) 
type Coord = (Char, Int)

-- | This function gets a String, contains coordinates
-- and returns 'Coord' value.
-- 
-- As example, for String @"d5"@ it's return @('d', 5)@.
coordFromFen :: String -> Coord
coordFromFen (x:y:[]) = (x, digitToInt y)

-- | This function gets a coordinates and returns it's 
-- string representation.
--
-- As example, for ('g',2) it's return @"g2"@.
coordToFen :: Coord -> String
coordToFen (x, y) = x: (show y)
