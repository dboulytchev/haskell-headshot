module Main where
import System.Environment
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Abstr = E | R | J Int Int deriving (Show,Eq)

solve instructions Nothing d [] (E:ys) = Just d
solve instructions Nothing _ xs (E:ys)  = Nothing
solve instructions Nothing _ [] (R:ys) = Nothing
solve instructions (Just _) _ _ (R:ys) = Nothing 
solve instructions Nothing d (x:xs) (R:ys) = solve instructions (Just x) d xs ys
solve instructions (Just 0) d xs ((J n l):ys) = solve instructions Nothing (d + n) xs (drop (l - 1) instructions)
solve instructions Nothing d xs ((J n l):ys) = Nothing 
solve instructions (Just a) d xs ((J n l):ys) = if (a > 0) then solve instructions (Just (a - 1)) d xs ys else Nothing

parse :: FilePath -> IO [Abstr]
parse path = do
    contents <- readFile path
    let labelMap = myMap contents
    return $ map (stringAbstr labelMap) (map (words . (separate [])) (lines contents))

separate :: [Char] -> [Char] -> [Char]
separate ys []      =  ys
separate ys (x:xs) = if x == ':' then xs else separate (ys ++ [x]) xs  

stringAbstr :: (Map [Char] Int) -> [[Char]] -> Abstr
stringAbstr labelMap [x]       = if (x == "r") then R else E
stringAbstr labelMap [x,y,z]   = J (read y :: Int) (fromJust (Map.lookup z labelMap))

myMap :: (Num a, Enum a) => String -> Map [Char] a
myMap xs  =  Map.fromList $ map (\(x,y) -> (takeWhile  (\z -> z /= ':') x,y)) (filter (or . map (\x -> x == ':') . fst) (zip  (lines xs) [1..]))

main :: IO ()
main = do 
    args <- getArgs
    let nameFile  = args !! 0
    let input = map (\x -> read x :: Int) $ tail args
    str <-  parse nameFile
    case solve str Nothing 0 input str of 
        Nothing  -> putStrLn "."
        (Just a) -> putStrLn $ show a
    return ()
