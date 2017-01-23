module EqClasses where

import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes (R r) = map notRep (foldl classEq [] r)
	where 
		notRep l = foldl (\acc x -> if (elem x acc) then acc else x:acc) [] l
		classEq::[[Int]] -> (Int,Int) -> [[Int]]
		classEq [] (a,b) = [[a,b]]
		classEq (x:xs) (a,b) = if (elem a x||elem b x) 
								then (a:b:x):xs 
								else [a,b]:x:xs
