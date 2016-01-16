module EqClasses where

import Data.List
import Rel
--newtype R = R {unR :: [(Int, Int)]} deriving Show

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes (R r) = map nub $ foldl eq [] r
     
eq [] x = [[fst x, snd x]] 
eq (y:ys) x = if (elem x1 y || elem x2 y) then (x1:x2:y):ys else [x1, x2]:y:ys where 
																				  x1 = fst x
																				  x2 = snd x
