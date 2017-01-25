import System.IO  
import System.Environment 
import Data.Char
import Data.List

-- Машинные инструкции

data Instruction n l = J n l | R | E   deriving Show

stringToInt = foldl (\x y -> (digitToInt y) + 10*x) 0 

--разбивает каждую строку на кортеж, в котором хранятся символы до : и после :, либо же вся строка и пустая строка 
-- в случае отсутствия :
parsSupport s = map (split ':') (lines s) where
 split d s = if ((sp1 /= "") && (sp2 /= "")) then ( sp1 , drop 1 sp2) else (sp1, sp2) where
			sp1 = fst $ span (/= d) s
			sp2 = snd $ span (/= d) s 

--синтаксический разбор, создание списка 2-элементных кортежей, в которых хранится информация
--  о наличии ссылки на строке и инструкция машины на строке	
parser instr = let 
	converse (i, "") = ("noLink", strToInstr (words i))
	converse (l, i) = (l, strToInstr (words i))
	strToInstr ["e"] = E
	strToInstr ["r"] = R
	strToInstr ["j", n, l] = J (stringToInt n) l  in map converse (parsSupport instr)
	
--вычисление результата, Nothing еквивалентно специальному значению регистра a   
takeRez _ Nothing  d [] (E:_)      = show d 	
takeRez i Nothing  d (x:dat) (R:ins)     = takeRez i (Just x) d dat ins 
takeRez i (Just 0) d dat  ((J n l):ins)  = takeRez i Nothing (d + n) dat (goToLink l i) 
takeRez i (Just a) d dat  ((J n l):ins)	 =  takeRez i (Just (a - 1)) d dat ins  
takeRez _ _ _ _ _ = "."

--вспомогательная функция перехода по ссылке для команды j  
goToLink s ((l, i):ts) = if l == s then map snd ((l, i):ts)
                               else goToLink s ts

main = do 
	input <- getArgs
	file <- readFile (head input)

	let instructions = parser file
	let inputInt = map stringToInt (tail input)
	let rez = takeRez instructions Nothing 0 inputInt (map snd instructions)
	putStrLn rez 