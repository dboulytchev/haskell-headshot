module Parse where

import Data.List
import Data.Char

data Command = E | R | J Integer (Label, Pointer)
	deriving Show

data Parsed  = Parsed (Label, Command)
	deriving Show

type Pointer = Integer
type Label   = Maybe String 

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
		com   = parseCom $ tail $ snd $ span (/= ':') str

parseWithoutLabel :: String -> Parsed
parseWithoutLabel str = Parsed (Nothing, com)
	where
		com = parseCom str

parseOneString :: String -> Parsed
parseOneString str = if (any (== ':') str) then parseWithLabel str else parseWithoutLabel str

parse :: FilePath -> IO [Parsed]
parse path = do
	content <- readFile path
	let res = map (\x -> parseOneString x) (lines content)
	return res
