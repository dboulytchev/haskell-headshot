import System.IO  
import System.Environment 
import Data.List.Split

-- data: 

data Command i s = E | R | J i s deriving Show

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
		
eval Nothing  d []        (E:_)         _ = Just d	
eval Nothing  d (i:input) (R:com)       s = eval (Just i) d input com s
eval (Just 0) d input     ((J i l):com) s = eval Nothing (d + i) input (findMark l s) s
eval (Just a) d input     ((J i l):com)	s = eval (Just (a - 1)) d input com s 
eval _        _ _          _            _ = Nothing
		
-- end evaluation

-- additional functions:  

parseAnswer Nothing  = "."
parseAnswer (Just x) = show x

-- end additional functions

main = do 
	args <- getArgs
	allFile <- readFile (head args)
	let intArgs = map (\x -> read x :: Int) (tail args)
	let listCommand = parse' allFile
	let answer = eval Nothing 0 intArgs (map snd listCommand) listCommand
	putStrLn (parseAnswer answer)