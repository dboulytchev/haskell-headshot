module Exec where


import Parser
import Dict
import Data.Map as Map

data Reg = Reg {a :: Int, d :: Int}

data Frame = Frame {reg :: Reg, idx :: Int} 

exec dict listOfInst input = runProg dict listOfInst (Frame (Reg (-1) 0) 0) input

runProg dict listOfInst frame@(Frame (Reg a d) idx) [] = case com $ listOfInst !! idx of
                   Exit True -> Just d 
                   Read True -> Nothing 
                   Jump True -> if a >= 0 
                                then runProg dict listOfInst (runJump dict (listOfInst !! idx) frame) []
                                else Nothing

runProg dict listOfInst frame@(Frame (Reg a d) idx) input@(curInput:xs) = case com $ listOfInst !! idx of
                                                      Exit True -> Nothing 
                                                      Read True -> if (a == -1 && input /= []) 
                                                                   then
                                                                     runProg dict listOfInst (Frame (Reg curInput d) $ idx + 1) xs
                                                                   else
                                                                     Nothing 
                                                      Jump True -> if a >= 0 
                                                                   then runProg dict listOfInst (runJump dict (listOfInst !! idx) frame) input
                                                                   else Nothing


runExit input (Frame (Reg a d) idx) | input == [] = Just d
                                    | otherwise   = Nothing

runJump dict inst (Frame (Reg a d) idx) | a == 0    = (Frame (Reg (-1) (d + (getNum $ num inst))) $ getIdx dict $ goTo inst)
                                        | otherwise = (Frame (Reg (a - 1) d) $ idx + 1)

getIdx dict (Just s) = case Map.lookup s dict of
                       Just num -> num

getNum (Just num) = num
