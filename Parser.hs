module Parser where
import Data.Text (pack, strip, unpack)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

type Label    = String
type LabelMap = Map.Map String [Command]

data Command = NOCOMMAND | EXIT | READ | JMP Int Label deriving (Show)

trimSpaces :: String -> String
trimSpaces = unpack . strip . pack

parseCommand :: String -> Command
parseCommand command = case trimSpaces command of
    "r" -> READ
    "e" -> EXIT
    jmp -> case words jmp of
       [_, acc, otherLabelName] -> JMP (read acc) otherLabelName

preprocess :: String -> (Maybe Label, Command)
preprocess line = case splitOn ":" $ trimSpaces line of
    [labelName, command] -> (Just labelName, parseCommand command)
    [command]            -> (Nothing       , parseCommand command)

text2ast :: [String] -> (LabelMap, [Command])
text2ast codeList = let
        fixLabels []              []           labelMap = labelMap
        fixLabels (Nothing:ls)    (_:cs)       labelMap = fixLabels ls cs labelMap
        fixLabels (Just label:ls) comms@(c:cs) labelMap = fixLabels ls cs (Map.insert label comms labelMap)

        (labels, commands) = unzip $ map preprocess codeList
    in (fixLabels labels commands Map.empty, commands)

parse :: String -> IO (LabelMap, [Command])
parse filename = do
    codeText <- readFile filename
    return $ text2ast $ lines codeText
