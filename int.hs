{-# OPTIONS_GHC -fno-warn-tabs #-}
import Data.List.Split
import System.Environment

data Flag = No_Flag | MkFlag String deriving (Eq, Show)
data Instr = Instr {flag :: Flag, cmd :: String, val :: Int, flagTo :: Flag} deriving Show
data A = NotInt | A Int deriving Eq
data Answer = Ans Int | Alarm
instance Show Answer where
	show Alarm = "."
	show (Ans x) = show x

infixl 1 &
(&) :: a -> (a -> b) -> b
x & f = f x

my_read :: String -> Int
my_read x = read x :: Int

parse :: String -> [Instr]
parse x = map parse2 $ filter (not.null) $ map (\j -> filter (/= "") j) $ map (splitOneOf " :\t") (splitOn "\n" x)

parse2:: [String] -> Instr
parse2 x = case x of 
					 (a : [])             -> Instr {flag = No_Flag, cmd = a}
				 	 (a : b : [])         -> Instr {flag = MkFlag a, cmd = b}
			 		 (a : b : c : [])     -> Instr {flag = No_Flag, cmd = a, val = (read b :: Int), flagTo = MkFlag c}
		 			 (a : b : c : d : []) -> Instr {flag = MkFlag a, cmd = b, val = (read c :: Int), flagTo = MkFlag d}
		 			 [] 				  -> error "Wrong Instruction"

findByFlag :: Flag -> [Instr] -> [Instr]
findByFlag f [] = error "No command with this flag"
findByFlag f (s : sp) | (s & flag) == f = (s : sp)
					  | otherwise = findByFlag f sp

iter :: [Instr] -> [Instr] -> A -> Int -> [Int] -> Answer
iter [] _ _ _ _ = Alarm

iter sp (x : xs) NotInt b arg| (x & cmd) == "e" = if (null arg) then Ans b
																else Alarm
						  	 | (x & cmd) == "r" = if (null arg) then Alarm
						  	 									else iter sp xs (A $ head arg) b (tail arg)
						  	 | otherwise = Alarm

iter sp (x : xs) (A a) b arg| (x & cmd) == "e" = Alarm
				  		 	| (x & cmd) == "r" = Alarm
				  		 	| (x & cmd) == "j" = if a == 0 then iter sp (findByFlag (x & flagTo) sp) NotInt (b + (x & val)) arg
				  								   		   else iter sp xs (A $ pred a) b arg
main = do
	args <- getArgs
	instrs <- readFile $ head args
	print(iter (parse instrs) (parse instrs) NotInt 0 (map my_read (tail args)))




