module Main where

import Data.Array
import Data.Functor
import Data.List
import Data.Ix
import Control.Applicative
import Control.Exception
import Text.Read

data Zelle = X | O | Empty
		deriving (Eq)

data Winner = Player1
            | Player2
            | Draw
            | NotFinished
		deriving (Show, Eq)

data Player = One
            | Two

instance Show Zelle where
    show X     = "X"
    show O     = "O"
    show Empty = " "

type Spielfeld = Array Position Zelle

type Position = (Int, Int)

emptyField :: Spielfeld
emptyField = array ((1,1),(3,3)) [((i,j),Empty) | i <- [1..3], j <- [1..3]]

testField :: Spielfeld
testField = listArray ((1,1),(3,3)) [X,O,X,
                                     X,O,O,
                                     X,Empty,Empty]

printField :: Spielfeld -> String
printField s = intercalate "\n--+---+--\n" [intercalate " | " $ show <$> [s ! (j,i) | i <- [1..3]] | j <- [1..3]] ++ "\n"

hasWon :: Spielfeld -> Winner
hasWon s
       | or ([hasRow, hasColumn, hasDiagonal] <*> [X] <*> [s]) = Player1
       | or ([hasRow, hasColumn, hasDiagonal] <*> [O] <*> [s]) = Player2
       | elem Empty $ elems s                                  = NotFinished
       | otherwise                                             = Draw

hasRow :: Zelle -> Spielfeld -> Bool
hasRow x s = or [all (==x) [s ! (j,i) | i <- [1..3]] | j <- [1..3]]

hasColumn :: Zelle -> Spielfeld -> Bool
hasColumn x s = or [all (==x) [s ! (i,j) | i <- [1..3]] | j <- [1..3]]

hasDiagonal :: Zelle -> Spielfeld -> Bool
hasDiagonal x s = all (==x) [s ! (i,i) | i <- [1..3]] ||
                  all (==x) [s ! (i,4-i) | i <- [1..3]]


leseEingabe :: Spielfeld -> IO Position
leseEingabe s = do
                 line <- getLine
                 pos <- case readMaybe line :: Maybe Position of
                        Just p -> return p
                        Nothing -> putStrLn "Fehlerhafte Eingabe." >> leseEingabe s
                 if not (inRange (bounds s) pos) || s ! pos /= Empty then
                   do putStrLn "Ungültiger Zug!"
                      leseEingabe s
                 else
                   return pos

playerToZelle :: Player -> Zelle
playerToZelle One = X
playerToZelle Two = O

nextPlayer :: Player -> Player
nextPlayer One = Two
nextPlayer Two = One

main :: IO ()
main = mainLoop emptyField One

mainLoop :: Spielfeld -> Player -> IO ()
mainLoop s p = do
             putStr $ printField s
             input <- leseEingabe s
             let s' = s // [(input, playerToZelle p)]
             case hasWon s' of
               Player1 -> putStrLn "Spieler 1 hat gewonnen!" >> putStr (printField s')
               Player2 -> putStrLn "Spieler 2 hat gewonnen!" >> putStr (printField s')
               Draw    -> putStrLn "Unentschieden!"          >> putStr (printField s')
               NotFinished -> mainLoop s' $ nextPlayer p




