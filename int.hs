import Data.List
import Data.Char
import System.Environment

data Flag = F String | NoFlag deriving Eq
data Command = E | R | J Int String
data Line = Line { flag :: Flag, command :: Command } 
data Result = Result Int | Interrupted

instance Show Result where
    show (Result x) = show x
    show _ = "."

aToInt (Just x) = x
fromStringToCommand :: [String] -> Command        
fromStringToCommand (x:(y:ys)) = case x of
    "j" -> J (read y) (concat ys)
    "r" -> R
    "e" -> E

fromStringToLine xs = Line { flag    = if null y then NoFlag else F x, 
                             command = if null y then fromStringToCommand . words $ x else fromStringToCommand . words . tail $ y }
    where (x,y) = span (/= ':') xs

seek [] y = []
seek next@(x:xs) y | flag x == y = next
                   | otherwise = seek xs y 

run :: [Line] -> [Line] -> [Int] -> Maybe Int -> Int -> Result
run lns (cur:next) [] a d = case command cur of
    R -> Interrupted
    E -> if a == Nothing then Result d else Interrupted
    J n l -> jumpHelper n (F l) a d
        where jumpHelper n l a d | a == Just 0 = run lns (seek lns l) [] Nothing (d + n)
                                 | a > Just 0 = run lns next [] (Just $ aToInt a - 1) d
                                 | otherwise = Interrupted
run lns (cur:next) stream a d = case command cur of
    E -> Interrupted
    R -> if a == Nothing then run lns next (tail stream) (Just . head $ stream) d else Interrupted
    J n l -> jumpHelper n (F l) a d
        where jumpHelper n l a d | a == Just 0 = run lns (seek lns l) stream Nothing (d + n)
                                 | a > Just 0 = run lns next stream (Just $ aToInt a - 1) d
                                 | otherwise = Interrupted
main = do
    args <- getArgs
    prog <- readFile (head args)
    let commands = lines prog
    let lns = map fromStringToLine commands
    print (run lns lns (map read (tail args)) Nothing 0)