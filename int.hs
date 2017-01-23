import System.Environment
import Data.Char

type ListArgs = [Int]
type RegD     = Int
type Result   = Maybe Int 

data RegA    = A Int | N deriving Eq
data Flag    = F String | NotF deriving Eq
data Command = ERROR | READ | JUMP Int Flag
data Line    = Line { flag :: Flag, command :: Command }


parsingCommands :: String -> Command
parsingCommands xs = case head . goToSym $ xs of
    'j' -> JUMP (read . takeWhile (isDigit) . startDrop $ xs) (F $ goToSym . dropWhile (isDigit) . startDrop $ xs)
    'r' -> READ
    'e' -> ERROR
    where 
        goToSym   = dropWhile (isSpace)
        startDrop = goToSym . tail . goToSym

parsingLines :: String -> Line
parsingLines xs = Line { flag    = if (null y) then NotF else F x, 
                         command = if (null y) then parsingCommands x else parsingCommands (tail y) }
    where (x, y) = span (/= ':') xs -- (True, False)

translateRegA :: RegA -> Int
translateRegA (A x) = x
translateRegA _     = (-1)

gotoFlag :: [Line] -> Flag -> [Line]
gotoFlag [] y = []
gotoFlag (x:xs) y | (flag x == y) = (x:xs)
                  | otherwise = gotoFlag xs y 

execute :: [Line] -> [Line] -> ListArgs -> RegA -> RegD -> Result
execute parsedCode (x:xs) listArgs a d = case command x of
    ERROR    -> if (a == N) && (null listArgs) then Just d else Nothing
    READ     -> if (a == N) && (listArgs /= []) then execute parsedCode xs (tail listArgs) (A (head listArgs)) d else Nothing
    JUMP n l -> jumper n l a d
        where jumper n l a d | (translateRegA a) < 0  = Nothing
                             | (translateRegA a) == 0 = execute parsedCode (gotoFlag parsedCode l) listArgs N (d + n)
                             | (translateRegA a) > 0  = execute parsedCode xs listArgs (A $ translateRegA a - 1) d

main :: IO()
main = do
   input <- getArgs
   let nameFile = head input
       progArg  = tail input
   code <- readFile nameFile
   let commands = lines code
       parsedCode = map parsingLines commands
       result = case (execute parsedCode parsedCode (map read progArg) N 0) of
                   (Just a) -> show a
                   (Nothing) -> "."
   putStrLn result
