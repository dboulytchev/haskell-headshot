module Main where
import System.Environment
import Data.Map (Map)
import Data.List (find)
import qualified Data.Map
import Data.Maybe

data Op = Read | Exit | Jump Int Int 
    deriving (Show, Eq)

main = do 
    args <- getArgs
    let pth  = head args
    let input = map (\x -> read x :: Int) $ (tail args)
    prog <- parse pth
    case solve Nothing 0 input prog prog of 
        Nothing  -> putStrLn "."
        (Just a) -> putStrLn (show a)

toOp labelMap ["r"] = Read 
toOp labelMap ["e"] = Exit
toOp labelMap ["j", y, z]  = 
    Jump (read y :: Int) (fromJust $ Data.Map.lookup z labelMap)

-- replaceme
afterSem ys []      =  ys
afterSem ys (x:xs)  = 
    if x == ':' then 
        xs 
    else 
        afterSem (ys ++ [x]) xs  

labelMap xs = Data.Map.fromList $ map f $ filter (pred . fst) enumerated where
    f (x, y) = (takeWhile (/= ':') x, y)
    pred x = case find (== ':') x of
        Nothing -> False
        (Just x) -> True
    enumerated = zip (lines xs) [1..]

parse f = do
    text <- readFile f
    let m = labelMap text
    return $ map (toOp m) $ map (words . (afterSem [])) $ lines text

solve Nothing d [] (Exit : rest) list = Just d
solve Nothing x xs (Exit : rest) list = Nothing
solve Nothing x [] (Read : rest) list = Nothing
solve (Just a) x y (Read : rest) list = Nothing
solve Nothing d xs ((Jump n l) : rest) list = Nothing
solve (Just 0) d xs ((Jump n l) : rest) list = solve Nothing (d + n) xs (drop (l - 1) list) list
solve (Just a) d xs ((Jump n l) : rest) list = if a < 0 then Nothing else solve (Just (a - 1)) d xs rest list
solve Nothing d (x:xs) (Read : rest) list = solve (Just x) d xs rest list
