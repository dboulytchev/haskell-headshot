import System.IO  
import System.Environment 
import Data.List.Split

-- data: 

data Command i s = T | E | R | J i s deriving Show

-- end data

-- parsing input:
	
parse' input = map interpriate (map (splitOn ":") (lines input)) where
	interpriate [l, c] = (Just l,  comm $ words c)
	interpriate [c]    = (Nothing, comm $ words c)
	comm ["e"]       = E
	comm ["r"]       = R
	comm ["j", i, s] = J (read i :: Int) s

-- end parsing input

-- evaluation:  

findMark s lst@((l, c):ls) = if l == Just s then map snd lst else findMark s ls

nxt [] = T			
nxt x = head x
			
eval a d output (R:com) s = case nxt com of
	(J _ _) -> eval 0 d output com s
	_ -> (maxBound :: Int, [])
eval a d output (E:com) s = (d, output)
eval a d output ((J i l):com) s = min q1 q2 where
	q1 = case nxt com of 
		(J _ _) -> eval (a + 1) d output com s
		_ -> (maxBound :: Int, [])
	q2 = let n = (findMark l s) in case nxt n of
		(J _ _) -> (maxBound :: Int, [])
		R -> eval 0 (d + i) (output ++ [a]) n s
		E -> (d + i, output ++ [a])
		T -> (maxBound :: Int, [])
		
-- end evaluation

main = do 
	args <- getArgs
	allFile <- readFile (head args)
	let listCommand = parse' allFile
	let answer = eval 0 0 [] (map snd listCommand) listCommand
	putStrLn (show answer)