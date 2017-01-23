module Exec where

import Parser
import Dict
import Data.Map as Map
import Data.Maybe as Maybe

data Reg = Reg {a :: Int, d :: Int}

data Frame = Frame {reg :: Reg, idx :: Int} 

exec dict listOfInst input = runProg dict listOfInst (Frame (Reg (-1) 0) 0) input

runProg dict listOfInst frame@(Frame (Reg a d) idx) [] = case listOfInst !! idx of
                   InstExit _ -> Just d 
                   InstRead _ -> Nothing 
                   InstJump _ _ _ -> if a >= 0 
                                 then runProg dict listOfInst (runJump dict (listOfInst !! idx) frame) []
                                 else Nothing

runProg dict listOfInst frame@(Frame (Reg a d) idx) input@(curInput:xs) = case listOfInst !! idx of
                                                      InstExit _ -> Nothing 
                                                      InstRead _ -> if (a == -1) 
                                                                then
                                                                  runProg dict listOfInst (Frame (Reg curInput d) $ idx + 1) xs
                                                                else
                                                                  Nothing 
                                                      InstJump _ _ _-> if a >= 0 
                                                                then runProg dict listOfInst (runJump dict (listOfInst !! idx) frame) input
                                                                else Nothing


runJump dict inst (Frame (Reg a d) idx) | a == 0    = (Frame (Reg (-1) (d + (num inst))) $ getIdx dict $ goTo inst)
                                        | otherwise = (Frame (Reg (a - 1) d) $ idx + 1)

getIdx dict s = fromJust $ Map.lookup s dict
 

