import System.Environment
import Data.Char
import Data.List

data TInstr = R | E | J (Int, String) deriving Show
type Instr = (Maybe String, TInstr)
type State = (Int, Int, Bool, Int, [Int])

getTInstr :: String -> TInstr
getTInstr str = 
  case head str of
    'r' -> R
    'e' -> E
    'j' -> J (getFirstArg, getSecondArg)
  where (preFirstArg, getSecondArg) = span isDigit (tail str)
        getFirstArg = read preFirstArg
    
parseCommand :: String -> Instr
parseCommand str = 
  case getLabel str of
    Nothing -> (Nothing, (getTInstr str))
    Just label -> ((Just label), getTInstr (str \\ (':' : label)))
  where getLabel str = 
          let res = if elem ':' str then takeWhile (/= ':') str else [] in
            if (null res) then Nothing else Just res

setIndices :: [Instr] -> [(Int, Instr)]
setIndices instr = zip ([1..length instr]) instr

getNextArg :: [Int] -> Int
getNextArg stdin = 
  if null stdin then (-1) else head stdin

getAddrByLabel :: Maybe String -> [(Int, Instr)] -> Int
getAddrByLabel jLabel cs = fst (head (filter (\(index, (lab, _)) -> lab == jLabel) cs))

evalAns :: [(Int, Instr)] -> [Int] -> State
evalAns cs stdin = runner ((-1), 0, True, 1, stdin)
  where
    runner :: State -> State
    runner state@(_, _, kw, _, _) = 
      if kw
      then runner (applyInstr state)
      else state
    
    applyInstr :: State -> State
    applyInstr (a, d, kw, toExec, stdin) = 
       case lookup toExec cs of
         Just (_, R) -> if (a == (-1)) && not (null stdin) 
         	            then ((getNextArg stdin), d, True, toExec + 1, tail stdin) 
         	            else ((-1), (-1), False, toExec + 1, stdin)
         Just (_, E) -> if (a == (-1)) && (null stdin) 
         	            then (a, d, False, toExec + 1, stdin) 
         	            else (a, (-1), False, toExec + 1, stdin)
         Just (_, J (arg1, arg2)) -> if a == 0 
         	                         then ((-1), d + arg1, True, getAddrByLabel (Just arg2) cs, stdin)
                                     else if (a == (-1)) || (a < 0)
                                          then (a, (-1), False, toExec + 1, stdin)
                                          else (a - 1, d, True, toExec + 1, stdin)
main = do
  args <- getArgs
  let file = head args
  let stdin = map read (tail args)
  code <- readFile file
  let commands = map (concat . words) $ lines code 
  let cs = setIndices $ map parseCommand commands
  let (_, ans, _, _, _) = evalAns cs stdin in
    case ans of
    (-1) -> print "."
    _ -> putStrLn (show ans)
  
