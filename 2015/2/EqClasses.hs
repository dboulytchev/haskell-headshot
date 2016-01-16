module EqClasses where

import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes (R xs) = map clearList $ foldl isIn [] xs
     where
	 clearList x = foldl (\acc x -> if (elem x acc) then acc else x:acc) [] x
	 isIn :: [[Int]] -> (Int, Int) -> [[Int]]
	 isIn [] x = [[fst x, snd x]]
	 isIn (y:ys) (a, b) = if (elem a y || elem b y) then (a:b:y):ys
                                                        else [a, b]:y:ys		 
