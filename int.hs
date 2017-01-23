module Main where

import System.Environment
import Data.List
import Data.Map as Map
import Data.Maybe

type ECode = Maybe Int 
type RegA = Maybe Int
type RegD = Int
type Input = [Int]
type LabelsMap = Map String Int

data State = State RegA RegD Int  
data InstructionType = E | R | J deriving (Show, Eq)
data ArgsType = None | ArgsType (Int, String) deriving Show
data Instruction = Instruction { label :: Maybe String,  instruction :: InstructionType, args :: ArgsType } deriving Show

stringToInt :: [String] -> [Int]
stringToInt = fmap read

getLabel :: String -> Maybe String
getLabel s =  if (isInfixOf ":" firstWord) then Just (init firstWord) else Nothing
              where firstWord = (words s) !! 0 
                
          
matchInstruction :: String -> InstructionType
matchInstruction "r" = R
matchInstruction "e" = E
matchInstruction "j" = J
                
getInstructionType :: String -> InstructionType
getInstructionType s = if (getLabel s == Nothing) then matchInstruction $ (words s) !! 0 else matchInstruction $ (words s) !! 1

getArguments :: String -> ArgsType
getArguments s | (getInstructionType s) == J && (getLabel s) == Nothing = ArgsType (stringToInt (words s) !! 1, (words s) !! 2)
               | (getInstructionType s) == J && (getLabel s) /= Nothing = ArgsType (stringToInt (words s) !! 2, (words s) !! 3)
               | otherwise = None

parseSouce :: [String] -> [Instruction]
parseSouce [] = []
parseSouce (x:xs) = Instruction (getLabel x) (getInstructionType x) (getArguments x) : parseSouce xs

getListOfJumps :: [String] -> Int -> [(String, Int)]  
getListOfJumps [] _ = []
getListOfJumps (x:xs) n = case getLabel x of
               Just s  -> ((s :: String), (n :: Int)) : (getListOfJumps xs $ n + 1)
               Nothing -> getListOfJumps xs $ n + 1


execute :: State -> [Instruction] -> Input -> LabelsMap ->  ECode
execute (State a d p) program input labelsMap | curInsrtuction == E && a == Nothing && (length input) == 0  = Just d   
                                              | curInsrtuction == E && a /= Nothing = Nothing
                                              | curInsrtuction == E && (length input) /= 0 = Nothing
                                              | curInsrtuction == R && a == Nothing && (length input) /= 0 = execute (State (Just (head input)) d (p + 1)) program (tail input) labelsMap
                                              | curInsrtuction == R && a /= Nothing = Nothing
                                              | curInsrtuction == R && (length input) == 0 = Nothing
                                              | curInsrtuction == J && a == Nothing = Nothing
                                              | curInsrtuction == J && valA == 0 = execute (State Nothing (n + d) lineLabel) program input labelsMap
                                              | curInsrtuction == J && valA > 0 = execute (State (Just (valA - 1)) d (p + 1)) program input labelsMap
                                              | otherwise = Nothing
                                              where
                                                (ArgsType (n, l)) = args (program !! p)
                                                (Just lineLabel) = Map.lookup l labelsMap
                                                (Just valA) = a
                                                curInsrtuction = instruction (program !! p)
                                               
                                
 

main = do
   args <- getArgs
   srcCode <- readFile (args !! 0)
   let linesOfFile = lines srcCode    
   let labelsMap = fromList $ (getListOfJumps linesOfFile 0)
   let program = parseSouce linesOfFile
   let input = stringToInt(tail args)
   let res = case execute (State Nothing 0 0) program input labelsMap of
                 (Just a) -> show a
                 (Nothing) -> "."
   putStrLn res
