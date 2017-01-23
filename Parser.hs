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
        fixLabels :: [Maybe Label] -> [Command] -> LabelMap -> [Command] -> (LabelMap, [Command])
        fixLabels [] [] labelMap out = (labelMap, reverse out)
        fixLabels (Nothing:xs)      (command:ys) labelMap out = fixLabels xs ys labelMap $ command : out
        fixLabels (Just label:xs) y@(command:ys) labelMap out = fixLabels xs ys (Map.insert label y labelMap) $ command : out

        (labels, commands) = unzip $ map preprocess codeList
    in fixLabels labels commands Map.empty []

parse :: String -> IO (LabelMap, [Command])
parse filename = do
    codeText <- readFile filename
    return $ text2ast $ lines codeText
