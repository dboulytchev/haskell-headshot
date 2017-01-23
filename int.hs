module Main where

import System.Environment
import Data.List
import Data.Map as Map
import Data.Maybe
import Parser


execute :: Stat -> [Instruction] -> Input -> LabelsMap ->  ExitCode
execute (Stat a d ip) program input labelsMap | curInsrtuction == Exit && a == Nothing && (length input) == 0  = Just d   
                                              | curInsrtuction == Read' && a == Nothing && (length input) /= 0 = execute (Stat (Just (head input)) d (ip + 1)) program (tail input) labelsMap
                                              | curInsrtuction == Jump && valA == 0                            = execute (Stat Nothing (n + d) lineLabel) program input labelsMap
                                              | curInsrtuction == Jump && valA > 0                             = execute (Stat (Just (valA - 1)) d (ip + 1)) program input labelsMap
                                              | otherwise                                                      = Nothing
                                              where
                                                (ArgsType (n, l)) = args (program !! ip)
                                                (Just lineLabel) = Map.lookup l labelsMap
                                                (Just valA) = a
                                                curInsrtuction = instruction (program !! ip)
                                               
main = do
   args <- getArgs
   fileContent <- readFile (args !! 0)
   let linesOfFile = lines fileContent 
   
   let labelsMap = fromList $ (getListOfJumps linesOfFile 0)
   let program = parseSouce linesOfFile
   let input = stringToInt(tail args)

   let res = case execute (Stat Nothing 0 0) program input labelsMap of
                 (Just a) -> show a
                 (Nothing) -> "."
   putStrLn res