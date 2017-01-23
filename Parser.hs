module Parser where
import Data.List
import Data.Map as Map

type ExitCode = Maybe Int 
type RegistrA = Maybe Int
type RegistrD = Int
type Input = [Int]
type LabelsMap = Map String Int


data Stat = Stat RegistrA RegistrD Int  
data InstructionType = Exit | Read' | Jump deriving (Show, Eq)
data ArgsType = None | ArgsType (Int, String) deriving Show
data Instruction = Instruction { label :: Maybe String,  instruction :: InstructionType, args :: ArgsType } deriving Show

stringToInt :: [String] -> [Int]
stringToInt = fmap read  

getLabel :: String -> Maybe String
getLabel line | isInfixOf ":" firstObj == True = Just $ init firstObj 
              | otherwise                       = Nothing
              where
                firstObj = (words line) !! 0 
                
          
matchInstruction :: String -> InstructionType
matchInstruction "r" = Read'
matchInstruction "e" = Exit
matchInstruction "j" = Jump
                
getInstructionType :: String -> InstructionType
getInstructionType line | getLabel line == Nothing = matchInstruction $ (words line) !! 0  
                        | otherwise                = matchInstruction $ (words line) !! 1

getArguments :: String -> ArgsType
getArguments line | (getInstructionType line) == Jump && (getLabel line) == Nothing = ArgsType (stringToInt (words line) !! 1, (words line) !! 2)
                  | (getInstructionType line) == Jump && (getLabel line) /= Nothing = ArgsType (stringToInt (words line) !! 2, (words line) !! 3)
                  | otherwise                                                       = None
  
     
parseSouce :: [String] -> [Instruction]
parseSouce [] = []
parseSouce (x:xs) = Instruction (getLabel x) (getInstructionType x) (getArguments x) : parseSouce xs


getListOfJumps :: [String] -> Int -> [(String, Int)]  
getListOfJumps [] _ = []
getListOfJumps (x:xs) n = case getLabel x of
  Just s  -> ((s :: String), (n :: Int)) : (getListOfJumps xs $ n + 1)
  Nothing -> getListOfJumps xs $ n + 1
