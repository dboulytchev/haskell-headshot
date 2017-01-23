{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Parse
import System.Environment
import Data.Maybe

type AMachine = (RegA, RegD)
type RegD = Integer
data RegA = A Integer | SpecV 

data Result = Fin AMachine Input | Exit | NextStep AMachine Label Input

type Input  = [Integer] 
type Answer = Maybe Integer

emptyInput = [-1]

machineWork :: AMachine -> [Instructions] -> Integer -> [Integer] -> Answer
machineWork m@(a, regD) code ip input =  
            case cmd of
                    Parsed (_, E) -> case a of
                          SpecV -> if (input == []) then Just regD else Nothing
                          _     -> Nothing

                    Parsed (_, R) -> case a of
                          SpecV -> if (input == []) then Nothing else machineWork (A (head input), regD) code (succ ip) (tail input)
                          _     -> Nothing

                    Parsed (_, J n (l, _)) -> case a of
                                    (A regA) -> if (regA == 0) 
                                                then machineWork (SpecV, regD + n) code (getIP code l) input
                                                else if (regA > 0) then machineWork (A (pred regA), regD) code (succ ip) input else Nothing
                                    _        -> Nothing
            where cmd :: Parsed = fromJust $ lookup ip code
							           

main = do 
    args <- getArgs
    prog_text <- Parse.parse $ args !! 0
    let input = map (\arg -> Parse.stringToInt arg) $ tail args

    let res = machineWork (SpecV, 0) prog_text 0 input
    case res of
    	Just x -> putStrLn $ show x
    	_      -> putStrLn $ show "."
