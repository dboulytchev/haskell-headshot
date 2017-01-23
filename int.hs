import Data.List
import Data.Char
import System.Environment
data RegisterA = A Int | N deriving Eq
data Flag = F String | NoFlag deriving Eq
data Command = E | R | J Int Flag
data Line = Line { flag :: Flag, command :: Command }
data Result = Result Int | Interrupted

instance Show Result where
    show (Result a) = show a
    show Interrupted = "."
aToInt (A x) = x
aToInt _ = -1

fromStringToCommand :: String -> Command
fromStringToCommand xs = case head . dropSp $ xs of
    'j' -> J (read . takeWhile (isDigit) . dropBegin $ xs) (F $ dropSp . dropWhile (isDigit) . dropBegin $ xs)
    'r' -> R
    'e' -> E
    where 
        dropSp = dropWhile (isSpace)
        dropBegin = dropSp . drop 1 . dropSp

fromStringToLine xs = Line { flag    = if null y then NoFlag else F x, 
                             command = if null y then fromStringToCommand x else fromStringToCommand (tail y) }
    where (x,y) = span (/= ':') xs

seek [] y = []
seek next@(x:xs) y | flag x == y = next
                   | otherwise = seek xs y 

run :: [Line] -> [Line] -> [Int] -> RegisterA -> Int -> Result
run lns (cur:next) stream a d = case command cur of
    E -> if (a == N) && (null stream) then Result d else Interrupted
    R -> if (a == N) && (not . null $ stream) then run lns next (tail stream) (A (head stream)) d else Interrupted
    J n l -> jumpHelper n l a d
        where jumpHelper n l a d | (aToInt a) < 0 = Interrupted
                                 | (aToInt a) == 0 = run lns (seek lns l) stream N (d + n) -- point = l
                                 | (aToInt a) > 0 = run lns next stream (A $ aToInt a - 1) d

int :: IO ()
int = do
    args <- getArgs
    prog <- readFile (head args)
    let commands = lines prog
    let lns = map fromStringToLine commands
    print (run lns lns (map read (tail args)) N 0)

main = int