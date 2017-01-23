module Dict where

import Data.Map as Map
import Parser
import Data.List as List
import Data.Maybe as Maybe

getDict = \xs -> Map.fromList (List.foldr (\(x, y) -> if Maybe.isJust $ label x then ([(fromJust $ label x, y :: Int)] ++) else ([] ++)) [] $ zip xs [0, 1 ..])

