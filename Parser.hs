module Parser where

import Data.List as Lst
import Data.Char

data Com = E | R | J {numb ::Int, label :: String} deriving Show -- ?

data Line = Line {lab:: String, command :: Com} deriving Show -- ?
type Programm = [Line]

parse :: FilePath -> IO[Line]
parse path =
  do
    contents <- readFile path
    return $ map toLine $ map words $ lines contents

toLine :: [String] -> Line
toLine s@(h:_) = if ':' `elem` h then Line{lab = init h, command = toCom (tail s)} 
                 else Line {lab = "", command = toCom s}


toCom :: [String] -> Com
toCom str = if length str == 1 
            then case head str of 
            	"e" -> E
            	"r" -> R   
            else J {numb = toInteg 0 (str !! 1), label = str !! 2}
             
toInteg :: Int -> String -> Int
toInteg a [] = a
toInteg a (x:xs) = toInteg (a * 10 + Data.Char.digitToInt x) xs              