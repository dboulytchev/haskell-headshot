import Data.List
import qualified Data.Map as Map
import System.Environment
import Data.Maybe
import Data.List.Split

-- Команда в программе
data Command = CommandE {flag :: Maybe String} |
               CommandR {flag :: Maybe String} |
               CommandJ {flag :: Maybe String , number :: Int, flagToGo :: String}

-- Делает из строки и флага в начале  команду
makeComm :: Maybe String -> [String] -> Command
makeComm lbl comm@(x:xs) =
  case x of
      "e" -> CommandE lbl
      "r" -> CommandR lbl
      "j" -> makeJump lbl comm where
        makeJump lbl (j:n:nl) = CommandJ lbl (read n :: Int) (head nl)

parse input = map makeCommands (map (splitOn ":") (lines input)) where
                       makeCommands [lbl, com] = makeComm (Just lbl) (words com)
                       makeCommands [com] = makeComm Nothing (words com)

-- Делает словарь по лэйблам - (лэйблб, номер в списке команд)
makeDictOfLabels :: [Command] -> Map.Map String Int
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
    let program = parse text
    let dict = makeDictOfLabels program
    let runTheComm numCom curRegA curRegD stStat =
          case (program !! numCom) of

            CommandE _  | (curRegA == Nothing && stStat > lenArgs) -> show curRegD
                        | otherwise -> "."

            CommandR _ | (curRegA == Nothing && stStat <= lenArgs) -> runTheComm (numCom + 1) (Just (read $ args !! stStat :: Int)) curRegD (stStat + 1)
                       | otherwise -> "."

            CommandJ _ num lst | (curRegA == Nothing) -> "."
                               | (fromJust curRegA == 0) -> runTheComm (fromJust $ Map.lookup lst dict) (Nothing) (curRegD + num)  stStat
                               | otherwise -> runTheComm (numCom + 1) (Just (fromJust curRegA - 1)) curRegD stStat

    do putStrLn (runTheComm 0 Nothing 0 1)
       return ()
