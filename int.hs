module Main where

import System.IO  
import System.Environment 
import Data.List


data Command n label = Exit | Read | Jump n label deriving (Show, Eq)


parse' :: String -> (Maybe String, Command Int String)
parse' line = parseStr (words line) where
  parseStr :: [String] -> (Maybe String, Command Int String) 
  parseStr [lbl , "e"]  = (Just (init lbl),  Exit)
  parseStr [lbl , "r"]  = (Just (init lbl),  Read)
  parseStr [lbl ,"j", n, label] = (Just lbl,  (Jump (read n :: Int) label)) 
  parseStr ["e"]  = (Nothing, Exit)
  parseStr ["r"]  = (Nothing, Read)
  parseStr ["j", n, label] = (Nothing, (Jump (read n :: Int) label ))

findPos :: String -> [(Maybe String, Command Int String)] -> [Command Int String]
findPos start list @ ((label, c) : tailLst) | label == Just start = fmap snd list 
                                            | otherwise = findPos start tailLst
findPos _ list = fmap snd list
      
execute :: Maybe Int -> Int -> [Int] -> [Command Int String] -> [(Maybe String, Command Int String)] -> Maybe Int   
execute (Just 0) d input     ((Jump i l) : com) allCmd = execute Nothing (d + i) input (findPos l allCmd) allCmd
execute (Just a) d input     ((Jump i l) : com) allCmd = execute (Just (a - 1)) d input com allCmd      
execute Nothing  d (i:input) (Read : com)       allCmd = execute (Just i) d input com allCmd
execute Nothing  d []        (Exit : _)         _ = Just d 
execute _        _ _          _            _ = Nothing
                
parseList :: [String] -> [(Maybe String, Command Int String)]
parseList [] = []
parseList (x:xs) = parse' x : parseList (xs)


printAns Nothing  = "."
printAns (Just x) = show x

main = do 
      args <- getArgs
      fileContent <- readFile (args !! 0)
      let input = map (\x -> read x :: Int) (tail args)
      let program = parseList (lines fileContent)
      let answer = (execute Nothing 0 input (fmap snd program) program) 
      putStrLn (printAns answer)