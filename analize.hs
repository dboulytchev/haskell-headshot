import Parser
import ExecAnalize
import Dict
import System.Environment
import Data.List as List 

main = 
  do 
  args  <- getArgs
  prog  <- readFile (args !! 0)
 -- let input = (tail args) 
  let parse = Parser.start prog
  let dict = Dict.getDict parse
  let ans = Exec.exec dict parse (List.map (read)  input)
  let solve = 
  case ans of
     Just x -> putStrLn (show x)
     Nothing -> putStrLn "."
