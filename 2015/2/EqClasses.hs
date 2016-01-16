module EqClasses where


import Rel
import Data.List

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes (R y) = main y [] where
  main [] y = y
  main (x: xs) y = main xs $ findClasses x y

  findClasses e []     = [[fst e, snd e]]
  findClasses e (x:xs) = if ((elem (fst e) $ x) || (elem (snd e) $ x))
                      then (nub ((fst e):(snd e):x)):xs
                      else x:(findClasses e xs)
                      
