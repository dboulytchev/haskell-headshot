import Data.List
import Data.Char
import Control.Monad
import System.IO
import System.Environment
import qualified Data.Map as Map

data Register = Register Int | Not 
  deriving (Show, Eq)
data State = State {currentLine :: Int, a_value :: Register, d_value :: Int, val_Input :: Int}
  deriving (Show, Eq)
  
data Command = E | R | J Int String
  deriving (Show, Eq)
  
  
zeroState  = State 0 Not 0 0
magicState = State 666 Not 666 666
  
--result :: FilePath -> FilePath -> IO (Maybe Int)
result programPath streamPath = do
                                 program <- getLinesList programPath
                                 steam   <- getLinesList streamPath
                                 return steam

                                 
                                 
 
main = do 
    args <- getArgs
    let fileName  = args !! 0
    let inputData = unwords $ tail args
    int inputData fileName
       
 
                           
eval' = main' >>= (\x -> putStrLn (fromRegister $ a_value x))                                 
                                
int input p = main'' input p >>= (\x -> putStrLn (fromRegister $ a_value x)) 
                                
fromRegister (Register x) = show x
fromRegister (Not) = "."
                                
main'  = readContents "test.input" >>= (\x -> (solve x))

solve :: String -> IO State
solve inp = getLinesList "test.p" >>= (\x -> return (newParse x inp 0 zeroState))

main'' input p = return input >>= (\x -> (solve' x p))

--solve :: String -> IO State
solve' inp p = getLinesList p >>= (\x -> return (newParse x inp 0 zeroState))
 
 
 
--parseProg :: [String] -> String -> State 
--parseProg fileLines inp = foldl (\st str -> parseLine fileLines inp str st) zeroState fileLines --parseLine inp "\t\tj 5 l1" (parseLine inp "\t\tr" zeroState)
                             
newParse fileLines inp line_numb oldState = parseProg' fileLines inp 1 (parseLine fileLines inp (fileLines !! line_numb) zeroState)
                             
                             
parseProg' fileLines inp line_numb oldState = if (length (words inp) < (val_Input oldState)) --(steamIsEmpty inp (val_Input oldState)) 
                                              then oldState 
                                              else let (State a b c d) = parseLine fileLines inp (fileLines !! line_numb) oldState in 
                                                    if (checkMagicState (State a b c d)) then (State a b c d) else
                                                     parseProg' fileLines inp (a) (State a b c d)

getLineByNumb :: [String] -> Int -> String
getLineByNumb fileLines n = fileLines !! n
                             
                             
parseLine :: [String] -> String -> String -> State -> State
parseLine fileLines inp str oldState = case getCommand $ if (':' `elem` str) then dropWhile (==':') str else str of
                            E -> case a_value oldState of 
                                    Register _ -> returnError
                                    Not        -> returnAns (d_value oldState)
                            R -> case a_value oldState of
                                    Register x -> returnError1
                                    Not        -> if (steamIsEmpty inp (val_Input oldState) == False) then (State (currentLine oldState + 1) (Register $ readIntFromInput inp (val_Input oldState)) (d_value oldState) (val_Input oldState + 1))
                                                                             else returnError
                            J n l -> case a_value oldState of 
                                        Register x ->  if (x == 0) then State (getLineByLabel fileLines l - 1) (Not) (d_value oldState + n) (val_Input oldState)
                                                                   else State (currentLine oldState + 1) (Register $ x - 1) (d_value oldState) (val_Input oldState)
                                        Not        -> returnErrorJ oldState
                                       

checkMagicState (State a b c d) = (a == 666) && (c == 666) && (d == 666)                                        

--returnAns ::                                     
returnAns x = State 666 (Register x) 666 666--error ("Ans " ++ (show x))

--returnError :: 
returnError = State 666 (Not) 666 666
returnError1 = State 666 (Not) 666 666
returnErrorJ x = State 666 (Not) 666 666

readIntFromInput  :: String -> Int -> Int
--readIntFromInput = undefined
readIntFromInput str val = str2Int $ (words str) !! val

getLineByLabel :: [String] -> String -> Int
getLineByLabel fileLines s = case Map.lookup s (createLabelMap fileLines) of
                     Nothing -> error "Nothing"
                     Just x -> x

steamIsEmpty ::  String -> Int -> Bool
steamIsEmpty str val = length (words str) <= val


createLabelMap :: [String] -> Map.Map String Int
createLabelMap s = createLabelMap' s s 

createLabelMap' :: [String] -> [String] -> Map.Map String Int
createLabelMap' _ []        = Map.empty
createLabelMap' strs (x:xs) = Map.union (setLabel strs (takeStringLabel x)) (createLabelMap' strs xs) 

setLabel :: [String] -> String -> Map.Map String Int
setLabel strs s = if (s == "") then Map.empty 
                               else Map.fromList ([(s, findLabel strs s)])

findLabel :: [String] -> String -> Int
findLabel strs s = snd $ head $ filter (\(x, y) -> isInfixOf (s ++ ":") x) (zip strs [1..])
                               
                               
takeStringLabel :: String -> String
takeStringLabel s = if (':' `elem` s) then takeWhile (/=':') s else ""
                               
getCommand :: String -> Command
getCommand s = if ('e' `elem` s) then E else
               if ('r' `elem` s) then R else
               J (fst args) (snd args) where args = getJInts s
               
getJInts :: String -> (Int, String)
getJInts s = let args = drop 2 $ dropWhile ( /= 'j') s in
               (str2Int (takeWhile (isDigit) args), drop 1(dropWhile (isDigit) args))
              

changeState :: State -> Command -> State
changeState = undefined 
              
getLinesList :: FilePath -> IO [String]
getLinesList path = readContents path >>= (\x -> return (lines x))
                                 
readContents :: FilePath -> IO String
readContents path = do  
                      handle <- openFile path ReadMode  
                      contents <- hGetContents handle 
                      return contents  
                      

powT t = t : powT (t * 10)
str2Int s = sum $ map (\(x, y) -> (digitToInt x) * y) (zip s (reverse $ take (length s) $ powT 1))
