module Main where

import Parse
import System.Environment
import Data.Maybe

data AMachine = M RegA RegD

data RegD = DVal Integer
data RegA = AVal Integer | SpecV 

data Result = Fin AMachine Input | Exit | NextStep AMachine Label Input

type Input        = [Integer] 
type Instructions = (Integer, Parsed)
type Answer       = Maybe Integer

machineStep :: AMachine -> Parsed -> [Integer] -> Result
machineStep m@(M regA regD@(DVal valD)) (Parsed (lbl, cmd)) input@(x:xs) = case cmd of
	E -> case regA of
		SpecV -> if (x == -1) then Fin m input else Exit
		_     -> Exit

	R -> case regA of
		SpecV -> if (x == -1) then Exit else NextStep (M (AVal x) regD) Nothing xs
		_     -> Exit

	J n (l, _) -> case regA of 
		(AVal valA) -> if (valA == 0) then NextStep (M SpecV (DVal (n + valD))) l input 
                                      else if (valA > 0) then NextStep (M (AVal (valA - 1)) regD) Nothing input else Exit
		_           -> Exit 

new_ip :: [Instructions] -> Label -> Integer -> Integer
new_ip cmd l ip = case l of
	Nothing -> ip + 1
	_       -> getIP cmd l

machineWork :: AMachine -> [Instructions] -> Integer -> [Integer] -> Answer
machineWork m code ip input = if null input 
								then case machineStep m cmd [-1] of
									Fin (M _ (DVal x)) _ -> Just x
									Exit                 -> Nothing
									NextStep nm lbl _    -> machineWork nm code (new_ip code lbl ip) []
								else case machineStep m cmd input of
									Fin (M _ (DVal x)) _ -> Just x
									Exit                 -> Nothing
									NextStep nm lbl out  -> machineWork nm code (new_ip code lbl ip) out
							where cmd = fromJust $ lookup ip code

getIP :: [Instructions] -> Label -> Integer
getIP code lbl = fst $ head $ filter (\(p, Parsed (mark, _)) -> mark == lbl) code

prepCmds :: [Instructions] -> [Instructions]
prepCmds cmds = map 
	(\(index, cmd) -> 
		case cmd of 
			Parsed (l, J x (lbl, _)) -> (index, Parsed (l, J x (lbl, getIP cmds lbl)))
			_                        -> (index, cmd)
	) 
	cmds

main = do 
    args <- getArgs
    prog_text <- Parse.parse $ args !! 0
    let instructions = prepCmds $ zip [0..] prog_text
    let input = map (\arg -> Parse.stringToInt arg) $ tail args

    let res = machineWork (M SpecV (DVal 0)) instructions 0 input
    case res of
    	Just x -> putStrLn $ show x
    	_      -> putStrLn $ show "."