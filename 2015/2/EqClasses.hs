module EqClasses where
import Data.List

import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes (R r) = nub [fil r k | k <- tlToList r]

fil :: [(Int, Int)] -> Int -> [Int]
fil r a = tlToList $ filter (tupCont a) r

tupCont :: Int -> (Int, Int) -> Bool
tupCont a (b,c) = a == b || a == c

tlToList :: [(Int, Int)] -> [Int]
tlToList s = nub $ fst us ++ snd us where
    us = unzip s