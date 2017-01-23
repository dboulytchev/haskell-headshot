module Dict where

import Data.Map as Map
import Parser

getDict = \xs -> fromList $ getList xs 0

getList [] _ = []
getList (x:xs) n = case label x of
                   Just s -> ((s :: String), (n :: Int)) : (getList xs $ n + 1)
                   Nothing -> getList xs $ n + 1  
