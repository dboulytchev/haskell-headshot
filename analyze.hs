{-# OPTIONS_GHC -fno-warn-tabs #-}
import Data.List.Split
import System.Environment

data Flag = No_Flag | MkFlag String deriving (Eq, Show)
data Instr = E Flag | R Flag | J Flag Int Flag deriving (Show, Eq)
data A = NotInt | A Int deriving Eq
instance Show A where
	show NotInt = "."
	show (A x) = show x
data Ans = Ans(Int, [Int]) deriving (Eq)
instance Ord Ans where
	Ans x <= Ans y = fst x <= fst y
instance Show Ans where
	show (Ans x) = "(" ++ show (fst x) ++ ", " ++ show (snd x) ++ ")"

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


fun :: Instr -> Bool
fun _ = False

iter :: [Instr] -> [Instr] -> A -> Int -> [Int] -> (Instr -> Bool) -> Ans

iter sp (x : xs) NotInt b num fu = case x of
										 (E f) -> Ans (b, num)
										 (R f) -> iter sp xs (A 0) b num (\r -> (fu r) || ((== x) r)) 
										 _ 	   -> Ans (maxBound :: Int, [maxBound :: Int])
iter sp (x : []) (A a) b num fu = case x of
											(J f nb toF) -> iter sp (findByFlag toF sp) NotInt (b + nb) (a : num) fu
iter sp (x : xs) (A a) b num fu = case x of
									(J f nb toF) -> case head xs of
										 				(J from val to) -> case (head $ findByFlag toF sp) of
										 										(E _) -> min (iter sp xs (A $ a - 1) b num fu) (iter sp (findByFlag toF sp) NotInt (b + nb) (a : num) fu)
										 										(R w) -> if (not $ fu $ R w) then min (iter sp xs (A $ a + 1) b num fu) (iter sp (findByFlag toF sp) NotInt (b + nb) (a : num) fu) else iter sp xs (A $ a - 1) b num fu
										 				(R t) -> if (xs == findByFlag toF sp) then iter sp (findByFlag toF sp) NotInt (b + nb) (a : num) fu else Ans (maxBound :: Int, [maxBound :: Int])
										 				(E t) -> if (xs == findByFlag toF sp) then iter sp (findByFlag toF sp) NotInt (b + nb) (a : num) fu else Ans (maxBound :: Int, [maxBound :: Int])
									_ -> Ans (maxBound :: Int, [maxBound :: Int])
main = do
	args <- getArgs
	instrs <- readFile $ head args
	let instL = parse instrs in print(iter instL instL NotInt 0 [] fun) 
