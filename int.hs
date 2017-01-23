import Data.List
import qualified Data.Map as Map
import System.Environment
import Data.Maybe

-- Команда в программе
data Command = CommandE {flag :: Maybe String} |
               CommandR {flag :: Maybe String} |
               CommandJ {flag :: Maybe String , number :: Int, flagToGo :: String}

-- Список команд
type Instructions = [Command]
type ProgramText = [String]

-- Делает команду из строки входного файла
getCommand :: ProgramText -> Command
getCommand comm@(x:xs) = let isFlag s = if (last s == ':') then Just $ init s else Nothing in
                             case isFlag x of
                               (Just lbl) -> makeComm (Just lbl) xs
                               Nothing    -> makeComm Nothing comm

-- Делает из строки и флага в начале  команду
makeComm :: Maybe String -> ProgramText -> Command
makeComm lbl comm@(x:xs) =
  case x of
      "e" -> CommandE lbl
      "r" -> CommandR lbl
      "j" -> makeJump lbl comm where
        makeJump lbl (j:n:nl) = CommandJ lbl (read n :: Int) (head nl)


-- Парсит входную программу на команды
parseInput :: String -> Instructions
parseInput prog = makeListOfCommands $ lines prog where
     makeListOfCommands prog = foldr (\ x xs -> (getCommand $ words x) : xs) [] prog


-- Делает словарь по лэйблам - (лэйблб, номер в списке команд)
makeDictOfLabels :: Instructions -> Map.Map String Int
makeDictOfLabels xs = Map.fromList $ makePairs xs 0 where
  makePairs [] _       = []
  makePairs (x:xs) num = case flag x of
                           Just smt -> (smt, num) : (makePairs xs $ num + 1)
                           Nothing  -> makePairs xs $ num + 1

main =
  do
    args <- getArgs
    text <- readFile $ args !! 0
    let lenArgs = length args - 1
    let program = parseInput text
    let dict = makeDictOfLabels program
    let runTheComm numCom curRegA curRegD stStat =
          case (program !! numCom)of

            CommandE _  | (curRegA == Nothing && stStat > lenArgs) -> show curRegD
                        | otherwise -> "."

            CommandR _ | (curRegA == Nothing && stStat <= lenArgs) -> runTheComm (numCom + 1) (Just (read $ args !! stStat :: Int)) curRegD (stStat + 1)
                       | otherwise -> "."

            CommandJ _ num lst | (curRegA == Nothing) -> "."
                               | (fromJust curRegA == 0) -> runTheComm (fromJust $ Map.lookup lst dict) (Nothing) (curRegD + num)  stStat
                               | otherwise -> runTheComm (numCom + 1) (Just (fromJust curRegA - 1)) curRegD stStat

    do putStrLn (runTheComm 0 Nothing 0 1)
       return ()
