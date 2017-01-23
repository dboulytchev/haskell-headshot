module Parser where
import Data.Text (pack, strip, unpack)
import Data.List.Split (splitOn)
import qualified Data.Map.Lazy as Map

type AST         = [String]
type LabelName   = String
type RawCodeLine = String
type Index       = Int
type LabelAddr   = Index
type LabelMap    = Map.Map LabelName LabelAddr
type RawListing  = [CommandLine]
type Listing     = [Command]

data Label       = NoLabel | LabelPromise LabelName | Label LabelAddr deriving (Show)
data Command     = NOCOMMAND | EXIT | READ | JMP Int Label            deriving (Show)
data CommandLine = Line Label Command                                 deriving (Show)

trimSpaces :: String -> String
trimSpaces = unpack . strip . pack

emptyLabelMap :: LabelMap
emptyLabelMap = Map.empty

emptyListing :: Listing
emptyListing = []

parseCommand :: String -> Command
parseCommand command = case trimSpaces command of
    "r" -> READ
    "e" -> EXIT
    jmp -> case words jmp of
       [_, acc, otherLabelName] -> JMP (read acc) $ LabelPromise otherLabelName

preprocess :: RawCodeLine -> CommandLine
preprocess line = case splitOn ":" $ trimSpaces line of
    [labelName, command] -> Line (LabelPromise labelName) $ parseCommand command
    [command]            -> Line NoLabel                  $ parseCommand command

enumerate :: [a] -> [(Index, a)]
enumerate = zip [0..]

text2ast :: [String] -> Listing
text2ast codeList = let
        getLabels :: [(Index, CommandLine)] -> LabelMap -> Listing -> (LabelMap, Listing)
        getLabels [] labelMap out = (labelMap, reverse out)
        getLabels ((_, Line NoLabel command):xs) labelMap out = getLabels xs labelMap $ command : out
        getLabels ((index, Line (LabelPromise labelName) command):xs) labelMap out =
            getLabels xs (Map.insert labelName index labelMap) $ command : out

        fixLabels :: LabelMap -> Listing -> Listing
        fixLabels labelMap = map (\command -> case command of
            JMP acc (LabelPromise labelName) -> JMP acc $ Label $ labelMap Map.! labelName
            command                          -> command)

        (labelMap, commands) = getLabels (enumerate $ map preprocess codeList) emptyLabelMap []
    in fixLabels labelMap commands



parse :: String -> IO Listing
parse filename = do
    codeText <- readFile filename
    return $ text2ast $ lines codeText
