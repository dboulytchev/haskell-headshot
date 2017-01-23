module Parse where

import Data.List
import Data.Char

data Command = E | R | J Integer (Label, Pointer)
	deriving Show

data Parsed  = Parsed (Label, Command)
	deriving Show

type Pointer = Integer
type Label   = Maybe String 
type Instructions = (Integer, Parsed)

prepCmds :: [Instructions] -> [Instructions]
prepCmds cmds = map 
	(\(index, cmd) -> 
		case cmd of 
			Parsed (l, J x (lable, _)) -> (index, Parsed (l, J x (lable, getIP cmds lable)))
			_                          -> (index, cmd)
	) 
	cmds

new_ip :: [Instructions] -> Label -> Integer -> Integer
new_ip cmd l ip = case l of
	Nothing -> ip + 1
	_       -> getIP cmd l

getIP :: [Instructions] -> Label -> Integer
getIP code lable = fst $ head $ filter (\(p, Parsed (mark, _)) -> mark == lable) code

stringToInt :: String -> Integer
stringToInt str = read str :: Integer

parseCom :: String -> Command
parseCom str = let cmds = words str in
	case length cmds of
		1 -> if (cmds !! 0) == "e" then E else R
		_ -> J (stringToInt $ cmds !! 1) (Just $ cmds !! 2, -1)

parseWithLabel :: String -> Parsed
parseWithLabel str = Parsed ((Just label), com)
	where 
		label = fst $ span (/= ':') str
		com   = parseCom $ concat $ tail $ words str

parseWithoutLabel :: String -> Parsed
parseWithoutLabel str = Parsed (Nothing, com)
	where com = parseCom str

parseOneString :: String -> Parsed
parseOneString str = if (any (== ':') str) then parseWithLabel str else parseWithoutLabel str

parse :: FilePath -> IO [Instructions]
parse path = do
	content <- readFile path
	let res = map (\x -> parseOneString x) (lines content)
	return $ prepCmds $ zip [0..] res
