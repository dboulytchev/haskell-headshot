import Data.List
import qualified Data.Map as Map
import System.Environment
import Data.Maybe

-- регистр A
data RegA = RegA {valA ::Int}

-- регистр D
data RegD = RegD {valD :: Int}

-- тип инструкции
data TypeOfCommand = E | R | J

-- Команда в программе
data Command = Command {flag :: Maybe String,
                        com :: TypeOfCommand,
                        number:: Maybe Int,
                        flagToGo :: Maybe String}

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
      "e" -> Command lbl E Nothing Nothing
      "r" -> Command lbl R Nothing Nothing
      "j" -> makeJump lbl comm where
        makeJump lbl (j:n:nl) = Command lbl J (Just (read n :: Int)) (Just $ head nl)


-- Парсит входную программу на команды
parseInput :: String -> Instructions
parseInput prog = makeListOfCommands $ lines prog where
     makeListOfCommands prog = foldr (\ x xs -> (getCommand $ words x) : xs) [] prog


data Execution = Finished String |
                 ReadStream {regD::RegD} |
                 Jump {regAj :: RegA, regDj :: RegD, lbl :: Maybe String}

-- выполянет одну команду программы
execComm :: Command -> RegA -> RegD -> Int -> Int -> Execution
execComm comm curRega curRegd numStr maxStr =
     case com comm of
          E | (valA curRega == (-1) && numStr > maxStr)
                  -> Finished $ show $ valD curRegd
            | otherwise -> Finished "."

          R | (valA curRega == (-1) && numStr <= maxStr)
                  ->ReadStream curRegd
            | otherwise ->  Finished "."

          J | (valA curRega == (-1)) -> Finished "."
            | (valA curRega == 0)
                  -> Jump (RegA (-1)) (RegD (valD curRegd +  fromJust ( number comm))) (flagToGo comm )
            | otherwise -> Jump (RegA (valA curRega - 1)) curRegd Nothing


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
          case execComm (program !! numCom) curRegA curRegD stStat lenArgs of
                       Finished x                     -> x
                       ReadStream d                   ->
                          runTheComm (numCom + 1) (RegA (read $ args !! stStat :: Int)) d (stStat + 1)
                       Jump (RegA (-1)) d (Just lst)  ->
                          runTheComm (fromJust $ Map.lookup lst dict) (RegA (-1)) d stStat
                       Jump a d _                     -> runTheComm (numCom + 1) a d stStat
    do putStrLn (runTheComm 0 (RegA (-1)) (RegD 0) 1)
       return ()
