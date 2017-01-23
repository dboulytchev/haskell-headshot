import System.IO  
import System.Environment   
import Data.List.Split    

-- data: 

data Command i s = E | R | J i s deriving Show
data Line m i s = Line {mark :: Maybe m, cmd :: Command i s} deriving Show
data State a d m1 i1 s1 m2 i2 s2 args = State {getA :: Maybe a, getD :: d, curCommands :: [Line m1 i1 s1], allCommands :: [Line m2 i2 s2], getArg :: [args]}

-- end data

-- parsing input:

parseCommand xs = let s = filter (/= []) (splitOn " " xs) in case s of
	["r"] -> R
	["e"] -> E
	x -> J (read (x !! 1) :: Int) (x !! 2)
	
toLine (x:y:[]) = Line {mark = Just x, cmd = parseCommand y}
toLine (x:[])   = Line {mark = Nothing, cmd = parseCommand x}
	
parseLines []     = []
parseLines (x:xs) = let p = splitOn ":" x in toLine p : parseLines xs

tabToSpace x = if x == '\t' then ' ' else x
parse xs = parseLines (lines (map tabToSpace xs)) 

-- end parsing input

-- evaluation:  

findMark s lst@(l:ls) = if mark l == Just s then lst else findMark s ls

eval state = evalCurCommand (cmd (head (curCommands state))) where
	evalCurCommand E = evalE (getA state) (getArg state) where
		evalE Nothing [] = Just (getD state)
		evalE _ _        = Nothing
	evalCurCommand R = evalR (getA state) (getArg state) where
		evalR Nothing (x:xs) = let newState = state {getA = Just x, curCommands = tail (curCommands state), getArg = xs} in eval newState
		evalR _ _            = Nothing
	evalCurCommand (J i s) = evalJ (getA state) where
		evalJ (Just 0) = let newState = state {getA = Nothing, getD = (getD state) + i, curCommands = findMark s (allCommands state)} in eval newState
		evalJ (Just x) = let newState = state {getA = Just (x - 1), curCommands = tail (curCommands state)} in eval newState
		evalJ Nothing  = Nothing
-- end evaluation

-- additional functions:  

parseAnswer Nothing  = "."
parseAnswer (Just x) = show x
 
toInt []     = []
toInt (x:xs) = (read x :: Int) : toInt xs 

-- end additional functions

main = do 
	args <- getArgs
	allFile <- readFile (head args)
	let intArgs = toInt (tail args)
	let listCommand = parse allFile
	let startState = State {getA = Nothing, getD = 0, curCommands = listCommand, allCommands = listCommand, getArg = intArgs}
	let answer = eval startState
	putStrLn (parseAnswer answer)