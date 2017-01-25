import System.IO  
import System.Environment 
import Data.List
import Data.Char
 
data Instruction n l = J n l | R | E   deriving Show

stringToInt = foldl (\x y -> (digitToInt y) + 10*x) 0 

parsSupport s = map (split ':') (lines s) where
 split d s = if ((sp1 /= "") && (sp2 /= "")) then ( sp1 , drop 1 sp2) else (sp1, sp2) where
			sp1 = fst $ span (/= d) s
			sp2 = snd $ span (/= d) s 
	
parser instr = let 
	converse (i, "") = ("noLink", strToInstr (words i))
	converse (l, i) = (l, strToInstr (words i))
	strToInstr ["e"] = E
	strToInstr ["r"] = R
	strToInstr ["j", n, l] = J (stringToInt n) l  in map converse (parsSupport instr)

goToLink s ((l, i):ts) = if l == s then map snd ((l, i):ts)
                               else goToLink s ts


takeRez i a d product (R:ins)  = case follow ins of
	{Just (J _ _) -> takeRez i 0 d product ins; _ ->  (maxBound :: Int, [])}
takeRez i a d product (E:ins) = (d, product)
takeRez i a d product ((J n l):ins) = let
  	one = case follow ins of 
		{Just (J _ _) -> takeRez i (a + 1) d product ins;_ ->  (maxBound :: Int, [])}
	p = (goToLink l i)
	two = case follow p of
		Just (J _ _) -> (maxBound :: Int, [])
		Just R -> takeRez i 0 (d + n) (product ++ [a]) p
		Just E -> (d + n, product ++ [a])
		Nothing -> (maxBound :: Int, []) 
       in min one two
		
follow [] = Nothing			
follow s = Just (head s)
			

main = do 
	input <- getArgs
	file <- readFile (head input)

	let instructions = parser file
	let rez = show $ takeRez instructions 0 0 [] (map snd instructions) 

	putStrLn rez