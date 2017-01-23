module Parse where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List.Split

data Instr = R | E | J Int Int deriving (Show,Eq)
type Text = [Instr]

parse :: FilePath -> IO Text
parse path = do
    contents <- readFile path
    let labelMap = makeMap contents
    return $ map (stringToInstr labelMap) (map (words . (dropPartList)) (lines contents))

dropPartList :: [Char] -> [Char]
dropPartList xs = case splitOn ":" xs of
    [x,y] -> y
    [x]   -> x

stringToInstr :: (Map [Char] Int) -> [[Char]] -> Instr
stringToInstr labelMap [x]       = if (x == "r") then R else E
stringToInstr labelMap [x,y,z]   = J (toInt y) (fromJust (Map.lookup z labelMap))

toInt s = read s :: Int

makeMap :: (Num a, Enum a) => String -> Map [Char] a
makeMap xs = Map.fromList $ map (\(x,y) -> (takeWhile (\z -> z /= ':') x,y)) (filter (or . map (\x -> x == ':') . fst) (zip (lines xs) [1..]))
