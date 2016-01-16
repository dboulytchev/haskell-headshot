module EqClasses where

import Rel
import Data.List
import Data.Tuple

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes (R r) = map nub (foldl classes' [] r) where
    classes' [] (x1,x2)      = [[x1, x2]]
    classes' (y:ys) (x1, x2) = if (elem x1 y || elem x2 y) then (x1:x2:y):ys else [x1, x2]:(y:ys)
