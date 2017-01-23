{-# OPTIONS_GHC -fno-warn-tabs #-}
import Data.List.Split
import System.Environment

data Flag = No_Flag | MkFlag String deriving (Eq, Show)
data Instr = E Flag | R Flag | J Flag Int Flag deriving (Show, Eq)
data A = NotInt | A Int deriving Eq
instance Show A where
	show NotInt = "."
	show (A x) = show x

parse :: String -> [Instr]
parse x = map parse2 $ filter (not.null) $ map (\j -> filter (/= "") j) $ map (splitOneOf " :\t") (splitOn "\n" x)

parse2:: [String] -> Instr
parse2 x = case x of
			   (a : []) 			-> if (a == "e") then E No_Flag else R No_Flag
			   (a : b : []) 		-> if (b == "e") then E $ MkFlag a else R $ MkFlag a
			   (a : b : c : []) 	-> J No_Flag (read b) $ MkFlag c
			   (a : b : c : d : []) -> J (MkFlag a) (read c) $ MkFlag d

findByFlag :: Flag -> [Instr] -> [Instr]
findByFlag f (s : sp) = if helper f s then (s : sp) else findByFlag f sp where
						   helper g (E x) = x == g
						   helper g (R x) = x == g
						   helper g (J x _ _) = x == g


iter :: [Instr] -> [Instr] -> A -> Int -> [Int] -> A
iter sp (x : xs) NotInt b arg = case x of
										 (E f) -> if (null arg) then A b
										 					  else NotInt
										 (R f) -> if (null arg) then NotInt
										 					  else iter sp xs (A $ head arg) b (tail arg)
										 u 	 -> NotInt
iter sp (x : xs) (A a) b arg = case x of
										 (E f) 	-> NotInt
										 (R f) 	-> NotInt
										 (J f nb toF)	-> if (a == 0) then iter sp (findByFlag toF sp) NotInt (b + nb) arg
										 						   else iter sp xs (A $ pred a) b arg
main = do
	args <- getArgs
	instrs <- readFile $ head args
	let instL = parse instrs in print(iter instL instL NotInt 0 (map read (tail args))) 
