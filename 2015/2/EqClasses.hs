module EqClasses where

import Data.List
import Rel

-- Написать функцию, которая по отношению (см. файл Rel.hs), которое
-- является отношением эквивалентности, строит список классов эквивалентности
classes :: R -> [[Int]]
classes (R y) = main y [] where
  main [] t = t
  main (x: xs) t = main xs $ findClasses x t

  findClasses e []     = [[fst e, snd e]]
  findClasses e (x:xs) = if ((elem (fst e) x) || (elem (snd e) x))
                      then (nub ((fst e):(snd e):x)):xs
                      else x:(findClasses e xs)
                      
