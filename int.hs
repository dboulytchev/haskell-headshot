import System.Environment
import Data.Map as M hiding (filter)
import Data.Char

data A x =  A x | Stop

data M x = M {a :: A x, d :: x}

data Instr = E | R  | L String | J Int String | Instr deriving Show


type Coms = Map Int Instr
type Labels = Map String Int
type Key = Int
--машина, поток, текущая строка, таблица команд, таблица меток
type Context = (M Int, [Int], Key, Coms, Labels)

main = do
	args<-getArgs
	let fileName = head args
 	input <- readFile fileName
	let num = drop 1 args
	let nums = Prelude.map(\x -> read x::Int) num
	let i = parseJ . words $ input
	let answer = eval (M Stop 0, nums, 1, stToMap $ i, labelsToMap $ i)
	case answer of
		Right d -> print d
		Left x -> print x
eval :: Context -> Either String Int
eval  context@(m, nums, key, coms, labels)  = do
					case (M.lookup key coms) of
						Just E -> evalE m nums
						Just R -> evalR context
						Just j@(J num label) -> evalJ j context
						_ -> Left$"."

evalE :: M Int -> [Int] -> Either String Int
evalE (M Stop d) [] = Right d
evalE _ _ = Left $ "." 

evalR :: Context -> Either String Int
evalR (M Stop d, (x:xs), key, coms, labels) = eval (M (A x) d, xs, key + 1, coms, labels)
evalR _ = Left $ "."

evalJ :: Instr -> Context ->  Either String Int
evalJ j ((M Stop d), _, _, _,_) = Left $ "."
evalJ (J n label) (M (A 0) d, nums, key, coms, labels) =
				 eval (M Stop (n + d), nums, x + 1, coms, labels) where 
					Just x = M.lookup label labels
evalJ j ((M (A a) d), nums, key, coms, labels) = 
				 eval (M (A (a - 1)) d, nums, key + 1, coms, labels) 

--сопоставим каждой команде - номер (для удобства перехода по меткам)
stToMap :: [String] -> Map Int Instr
stToMap prog = fromList (zip [1.. length prog] (Prelude.map stToInstr prog))

--отдельная map для меток
labelsToMap :: [String] -> Map String Int
labelsToMap prog = fromList $ filter (\(x,y) -> ':' `elem` x) (zip prog [1..length prog])

stToInstr :: String -> Instr
stToInstr "r" = R
stToInstr "e" = E
stToInstr ('j':xs) = J x y where
			xs' = words xs
			x = read$head xs' :: Int
			y = last xs' ++ ":"
stToInstr label = L label

parseJ :: [String] -> [String]
parseJ (x:y:z:xs) = if (x == "j") then (x++" "++y++" "++z):(parseJ xs)
		    else x:parseJ(y:z:xs) 
parseJ xs = xs
