module EqClasses where

import Data.List
import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes r@(R l) = nub $ map (\x -> relWith r x) $ flat l

flat :: [(Int, Int)] -> [Int]
flat l = nub $ uncurry (++) $ unzip l

relWith :: R -> Int -> [Int]
relWith (R r) x = flat $ filter (\(a, b) -> a == x || b == x) r
