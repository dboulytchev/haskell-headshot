module EqClasses where

import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes r = map nub $ foldl classEq [] (unR r) where 
	сlassEq [] (a,b) = [[a, b]] 
	classEq l@(y:ys) (a, b) = if (a `elem` y || b `elem` y) then (a:b:y):ys else [a, b]:l
