import Parser
import Exec
import System.Environment


main = do 
	   args <- getArgs
	   let path = args !! 0
	   let input = tail args
	   paersed <- Parser.parse path
	   ex      <- Exec.result paersed input
	   return ()