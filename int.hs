import Parser
import Exec
import Dict
import System.Environment
import Data.List as List 

main = 
  do 
  args  <- getArgs
  prog  <- readFile (args !! 0)
  let input = (tail args) 
  let parse = Parser.start prog
  let dict = Dict.getDict parse
  let ans = Exec.exec dict parse (List.map (read)  input)
  case ans of
     Just x -> putStrLn (show x)
     Nothing -> putStrLn "."
