module Test where

import Piece
import Board

testingBoard1 = 
      addPiece ('a',1) (Piece White Rook) 
	$ addPiece ('b',1) (Piece White Knight) 
	$ addPiece ('c',1) (Piece White Bishop) 
	$ addPiece ('d',1) (Piece White Queen) 
	$ addPiece ('e',1) (Piece White King) 
	$ addPiece ('f',1) (Piece White Bishop)
	$ addPiece ('g',1) (Piece White Knight) 
	$ addPiece ('h',1) (Piece White Rook) 
	emptyBoard

testingBoard2 = boardFromFen "4k3/8/4K3/8/8/8/R7/8"

testingBoard3 = newBoard

startingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

test b = do
	putStr $ displayBoard b
	putStrLn $ (boardToFen b) ++ "\n"

assert name value = if value then putStrLn (name ++ " Ok!") else putStrLn (name ++ "Error!")

main = do
	mapM_ (test) [testingBoard1, testingBoard2, testingBoard3]

	assert "newBoard test" $ newBoard == boardFromFen startingFen
	assert "parsing and generation test" $ startingFen == (boardToFen . boardFromFen $ startingFen)
