module EqClasses where

import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes (R []) = [[]]
classes (R ((x, y) : []))= if (x == y ) then [[x]] else [[x, y]] 
classes (R ((x, y) : xs)) = 
   if (null filt) then if (x == y) then [x] : all else [x, y] : all 
                  else map (\a -> if ((x `elem` a) || (y `elem` a)) then  addd (addd a x) y else a) all
   where all =  classes (R xs)
         filt = filter (\a ->  (x `elem` a) || (y `elem` a)) all
         addd a x  = if (x `elem` a) then a else x : a 
