module EqClasses where

import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
lists [] x |(fst x == snd x) = [[fst x]] 
           | otherwise = [[fst x, snd x]]
lists (y:ys) x | (elem (snd x) y) && (elem (fst x) y) = y:ys
	       | (elem (fst x) y) = ((snd x):y):ys
               | (elem (snd x) y) = ((fst x):y):ys
               | otherwise = y:(lists ys x)
               
classes (R x) = classesM x [] where
    classesM :: [(Int, Int)] -> [[Int]] -> [[Int]]
    classesM [] y = y
    classesM (x:xs) y  = classesM xs (lists y x)