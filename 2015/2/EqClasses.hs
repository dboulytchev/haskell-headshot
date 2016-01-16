module EqClasses where

import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]

classes (R x) = makeList x [[]] where
    makeList :: [(Int, Int)] -> [[Int]] -> [[Int]]
    makeList [] y = y
    makeList (x:xs) y  = makeList xs (allLists y x1 x2)
             where 
                 (x1, x2) = x
                 allLists [[]] x1 x2 |(x1 == x2) = [[x1]] 
                                     | otherwise = [[x1, x2]]
                 allLists [y]  x1 x2 | (elem x1 y), (not $ elem x2 y) = [x2:y] 
                                     | (elem x2 y), (not $ elem x1 y) = [x1:y] 
                                     | (elem x2 y), (elem x1 y)       = [y] 
                                     | otherwise = y:(allLists [[]] x1 x2)
                    
                 allLists (y:ys) x1 x2 | (elem x1 y), (not $ elem x2 y) = (x2:y):ys
                                       | (elem x2 y), (not $ elem x1 y) = (x1:y):ys
                                       | (elem x2 y), (elem x1 y)       = y:ys
                                       | otherwise = y:(allLists ys x1 x2) 
                 


  