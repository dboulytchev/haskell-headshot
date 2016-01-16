module EqClasses where

import Rel
import Data.List

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes (R l) = map nub . delete [] $ classes' l

classes' :: [(Int,Int)] -> [[Int]]
classes' l = 
	case l of
		[] -> [[]]
		(a,b):k -> let (x,y) = findEq [a,b] k in
			x : (classes' y)

--findEq :: listToAddPair -> list -> (ansList, newList)
findEq :: [Int] -> [(Int,Int)] -> ([Int], [(Int,Int)])
findEq ans list = 
	case list of
		[] -> (ans, list)
		_ -> let (an, nlist, w) = findEq' ans list [] False in
				if not w then (an, nlist)
				else findEq an nlist
				
--findEq' :: listToAddPair -> olist -> nlist -> wasFount -> (newListToAddPair, newList, wasFoundNew)
findEq' :: [Int] -> [(Int,Int)] -> [(Int,Int)] -> Bool -> ([Int], [(Int,Int)], Bool)
findEq' ans olist nlist w = 
	case olist of
		[] -> (ans, nlist, w)
		qu@(q,u):r -> if (q `elem` ans || u `elem` ans) --(a == q || b == q || a == u || b == u) 
					then findEq' (q:u:ans) r nlist True
					else findEq' ans r (qu:nlist) w