module EqClasses where

import Rel
import Data.List

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes (R x) = foo x [] where
	foo :: [(Int, Int)] -> [[Int]] -> [[Int]]
	foo [] y 						= y 
	foo (x:xs) y					= foo xs (incl x y)

	incl :: (Int, Int) -> [[Int]] -> [[Int]]
	incl x [] 		= [[fst x, snd x]]
	incl x (y:ys) 	= if (elem (fst x) y) || (elem (snd x) y) 
						then (nub ((fst x):(snd x):y)):ys
						else y:(incl x ys)
