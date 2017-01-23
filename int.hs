import Data.List
import qualified Data.Map as Map
import System.Environment
import Data.Maybe
import Data.List.Split

data Command = E {flag :: Maybe String} |
               R {flag :: Maybe String} |
               J {flag :: Maybe String , num :: Int, lbl :: String}


makeComm :: Maybe String -> [String] -> Command
makeComm lbl comm@(x:xs) =
  case x of
      "e" -> E lbl
      "r" -> R lbl
      "j" -> makeJump lbl comm where
        makeJump lbl (j:n:nl) = J lbl (read n :: Int) (head nl)

parse input = map makeCommands (map (splitOn ":") (lines input)) where
                       makeCommands [lbl, com] = makeComm (Just lbl) (words com)
                       makeCommands [com] = makeComm Nothing (words com)

-- Делает словарь по лэйблам - (лэйблб, номер в списке команд)
lbls' :: [Command] -> Map.Map String Int
lbls' xs = Map.fromList $ makePairs xs 0 where
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
    let dict = lbls' program
    let runTheComm index a d stStat =
          case (program !! index) of

            E _  | (a == Nothing && stStat > lenArgs) -> show d
                        | otherwise -> "."

            R _ | (a == Nothing && stStat <= lenArgs) -> runTheComm (index + 1) (Just (read $ args !! stStat :: Int)) d (stStat + 1)
                       | otherwise -> "."

            J _ num lst | (a == Nothing) -> "."
                               | (fromJust a == 0) -> runTheComm (fromJust $ Map.lookup lst dict) (Nothing) (d + num)  stStat
                               | otherwise -> runTheComm (index + 1) (Just (fromJust a - 1)) d stStat

    do putStrLn (runTheComm 0 Nothing 0 1)
       return ()
