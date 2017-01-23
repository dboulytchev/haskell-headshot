module Rel where

import Data.List
import Data.Tuple

-- Бинарное отношение на подмножестве целых чисел задано
-- списком пар без повторений
newtype R = R {unR :: [(Int, Int)]} deriving Show

-- Реализовать сравнение отношений на равенство (как множеств)
instance Eq R where
  (==) (R r1) (R r2) = (sort r1) == (sort r2) 

-- sub r1 r2 == True <=> r1 --- подмножество r2
sub :: R -> R -> Bool
sub (R r1) (R r2) =  sub' r1 r2 where
    sub' [] []     = True
    sub' [] ys     = True
    sub' (x:xs) ys = (elem x ys) && (sub' xs ys)

-- dom r --- список (без повторений) целых чисел, каждое из
-- которых имеет хотя бы одно вхождение в r
dom :: R -> [Int]
dom (R r) = nub $ (map fst r) ++ (map snd r)

-- add r1 r2 --- объединение r1 и r2 как множеств (в результате
-- должен получиться список без повторений)
add :: R -> R -> R
add (R r1) (R r2) = R . nub $ r1 ++ r2
-- rev r --- отношение, "обратное" к r (т.е. в каждой паре надо
-- компоненты поменять местами).
rev :: R -> R
rev (R []) = R []
rev (R ((x,y) : xys)) = R (ys ((x,y) : xys)) where
    ys []               = []
    ys ((x1,y1) : xy1s) = (y1,x1) : ys xy1s

-- join r1 r2 --- отношение, состоящее из тех и только тех пар
-- (x, y), для которых существует z, такое, что (x, z) принадлежит
-- r1 и (z, y) принадлежит r2
join :: R -> R -> R
join (R r1) (R r2) = R (join' r1 r2) where
    join' r1 r2 = [(x,y) | (x,z) <- r1, (z',y) <- r2, z' == z] 

-- closure r строит транзитивное замыкание отношения r (т.е. наименьшее
-- транзитивное отношение, содержащее r)
closure :: R -> R
closure (R r) = if length r == length c then (R r)  else closure (R c)
  where c = nub [ (x1,y2) | (x1,y1) <- r, (x2,y2) <- r, y1 == x2 || (x1 == x2 && y1 == y2) ]

-- isReflexive r == True <=> r --- рефлексивное отношение на dom r
isReflexive :: R -> Bool
isReflexive (R r) = foldr (&&) True (map (\(x,_) -> (x,x) `elem` r) r)

-- isSymmetric r == True <=> r --- симметричное отношение
isSymmetric :: R -> Bool
isSymmetric (R r) = foldr (&&) True (map (\(x,y) -> (y,x) `elem` r) r)

-- isTransitive r == True <=> r --- транзитивное отношение
isTransitive :: R -> Bool
isTransitive r = r == closure r

-- isEquivalence r == True <=> r --- отношение эквивалентности
isEquivalence :: R -> Bool
isEquivalence r = (isReflexive r) && (isSymmetric r) && (isTransitive r)
--for restart