module EqClasses where

import Data.List
import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes r = map nub $ foldl in_class [] (unR r) where
 	 in_class [] (a,b) = [[a, b]]
 	 in_class list@(y:ys) (a, b) = if (elem a y || elem b y) then (a:b:y):ys else [a, b]:list
	 