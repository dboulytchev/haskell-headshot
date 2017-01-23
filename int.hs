module Main where

import System.Environment
import Data.List as List
import Data.Maybe
import Data.Map as Map

data Instruction = InstructionE | InstructionR | InstructionJ Int Label deriving Show
type Label = String
type InstructionIndex = Int
type InstructionsList = [Instruction]
type Jumps = Map Label InstructionIndex
type Program = (InstructionsList, Jumps)
type InputStream = [Int]

type Result = Either Int Char

type RegA = Maybe Int

data MachineState = MachineState {
  registerA :: RegA,
  registerD :: Int,
  rip :: Int
} deriving Show

startMachineState = MachineState {
  registerA = Nothing,
  registerD = 0,
  rip = 0
}


{-PARSING-}

parseProgram :: String -> Program
parseProgram fileContent = let h = List.map words (lines fileContent)
   in (createInstructionsList h, Map.fromList $ createJumps h) 

createJumps linesAsLists = helper 0 linesAsLists
    where 
      helper _ [] = []
      helper i (x:xs) | hasLabel x = (takeWhile (/= ':') $ head x, i) : helper (i + 1) xs
                      | otherwise = helper (i + 1) xs

createInstructionsList linesAsLists = List.map (\x -> createInstruction (if hasLabel x then tail x else x)) linesAsLists
    where createInstruction l | head l == "r" = InstructionR
                              | head l == "j" = InstructionJ (read (l !! 1)) (l !! 2)
                              | head l == "e" = InstructionE

hasLabel x = elem ':' $ head x

{-/PARSING-}

{-EXECUTING-}

updateRegADRip x a d r = x {registerA = a, registerD = d, rip = r}
updateRegA x a = x {registerA = a}
incRegRip x = x {rip = rip x + 1}
decRegA x = x {registerA = Just (fromJust (registerA x) - 1)}

execute :: Program -> InputStream -> MachineState -> Result
execute program inputStream machineState = 
  case fst program !! (rip machineState) of
    InstructionR -> doRead program inputStream machineState
    InstructionJ n l -> doJump program n l inputStream machineState
    InstructionE -> doExit inputStream machineState                       

doJump program n l inputStream machineState | a == Just 0                = execute program inputStream (updateRegADRip machineState Nothing (n + registerD machineState) (fromJust (Map.lookup l $ snd program)))  
                                            | isJust a && fromJust a > 0 = execute program inputStream (decRegA . incRegRip $ machineState)
                                            | otherwise                  = Right '.'  
  where a = registerA machineState 

doRead program inputStream machineState | (isNothing $ registerA machineState) && (not $ List.null inputStream) = execute program (tail inputStream) (updateRegA (incRegRip machineState) $ Just (head inputStream)) 
                                        | otherwise                                                             = Right '.'

doExit inputStream machineState | List.null inputStream && (isNothing $ registerA machineState) = Left (registerD machineState)    
                                | otherwise                                                     = Right '.'


{-/EXECUTING-}


printResult (Right x) = putStrLn [x]
printResult (Left x) = putStrLn $ show x

main = do args <- getArgs
          fileContent <- readFile (args !! 0)
          let inputStream = List.map read $ tail args
          let program = parseProgram fileContent
          let result = execute program inputStream startMachineState
          printResult result
