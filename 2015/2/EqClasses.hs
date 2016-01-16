module EqClasses where

import Data.List
import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes (R r) = nub $ map (\x -> relWith (R r) x) $ flat r

flat :: [(Int, Int)] -> [Int]
flat l = sort $ nub $ uncurry (++) $ unzip l

relWith :: R -> Int -> [Int]
relWith (R r) x = flat $ filter (\(a, b) -> a == x || b == x) r
