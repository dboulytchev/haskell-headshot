import System.Environment
import Data.Char
import Data.List
import Data.Maybe

type Stdin = [Int]
data TInstr = R | E | J {getInt :: Int, getNext :: String} deriving Show
type Label = Maybe String
data Instr = Instr Label TInstr deriving Show
type CS = [(Int, Instr)]

type A = Maybe Int -- bottom == Nothing
type D = Maybe Int -- '.' == Nothing
type Kw = Bool --keep working
data State = State {getA :: A, getD :: D, getKw :: Kw, toExec :: Int, getStd :: Stdin} deriving Show

getTInstr :: String -> TInstr
getTInstr str = 
  case head str of
    'r' -> R
    'e' -> E
    'j' -> J getFirstArg getSecondArg
  where
    (preFirstArg, getSecondArg) = span isDigit (tail str)
    getFirstArg = read preFirstArg
    

parseCommand :: String -> Instr
parseCommand str = 
  case getLabel str of
    Nothing -> Instr Nothing (getTInstr str)
    Just label -> Instr (Just label) $ getTInstr (str \\ (':' : label))
  where 
    getLabel str = 
      let res = if elem ':' str 
      	        then takeWhile (/= ':') str 
      	        else []
      in
        if (null res)
        then Nothing
        else Just res

setIndices :: [Instr] -> CS
setIndices instr = zip ([1..length instr]) instr

getNextArg :: Stdin -> Maybe Int
getNextArg stdin = 
  if null stdin
  then Nothing
  else Just (head stdin)

getAddrByLabel :: Maybe String -> CS -> Int
getAddrByLabel jLabel cs = fst (head (filter (\(index, Instr lab _) -> lab == jLabel) cs))

evalAns :: CS -> Stdin -> State
evalAns cs stdin = runner (State Nothing (Just 0) True 1 stdin)
  where
    runner :: State -> State
    runner state = 
      if (getKw state)
      then runner (applyInstr state)
      else state
    
    applyInstr :: State -> State
    applyInstr state = 
      let com = lookup (toExec state) cs
          stA = getA state 
          stD = getD state
          stExec = toExec state
          stStdin = getStd state
      in
       case com of
         Just (Instr _ R) -> if (stA == Nothing && not (null stStdin))
    	                     then State (getNextArg stStdin) stD True (stExec + 1) (tail stStdin)
    	                     else State Nothing Nothing False (stExec + 1) stStdin
         
         Just (Instr _ E) -> if ((stA == Nothing) && (null stStdin))
    	                     then State Nothing stD False (stExec + 1) stStdin
    	                     else State Nothing Nothing False (stExec + 1) stStdin
         
         Just (Instr _ (J arg1 arg2)) -> 
               if (stA == Just 0)
               then State Nothing (Just ((fromJust stD) + arg1)) True (getAddrByLabel (Just arg2) cs) stStdin
               else if ((stA == Nothing) || ((fromJust stA) < 0))
	                then State Nothing Nothing False (stExec + 1) stStdin
	                else State (Just ((fromJust stA) - 1)) stD True (stExec + 1) stStdin

main = do
  args <- getArgs
  let file = head args
  let stdinStr = tail args
  let stdin = map read stdinStr
  code <- readFile file
  let commands = map (concat . words) $ lines code 
  let cs = setIndices $ map parseCommand commands
  let ans = evalAns cs stdin
  case (getD ans) of
    Nothing -> print "."
    Just x -> putStrLn (show x)

  