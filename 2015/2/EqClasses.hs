module EqClasses where

import Data.List
import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes r = map nub $ foldl toClass [] (unR r) where
    toClass [] (a,b) = [[a, b]]
    toClass list@(y:ys) (a, b) = if (a `elem` y || b `elem` y) then (a:b:y):ys else [a, b]:list