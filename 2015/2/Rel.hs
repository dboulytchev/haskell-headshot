module Rel where

import Data.List
import Data.Tuple

-- Бинарное отношение на подмножестве целых чисел задано
-- списком пар без повторений
newtype R = R {unR :: [(Int, Int)]} deriving Show

-- Реализовать сравнение отношений на равенство (как множеств)
instance Eq R where
  R r1 == R r2 = (sort r1) == (sort r2)

-- sub r1 r2 == True <=> r1 --- подмножество r2
--sub :: R -> R -> Bool
sub (R r1) (R r2) = [x | x <- r1, x `elem` r2] == r1

-- dom r --- список (без повторений) целых чисел, каждое из
-- которых имеет хотя бы одно вхождение в r
dom :: R -> [Int]
dom (R r) = nub $ (map fst r) ++ (map snd r)

-- add r1 r2 --- объединение r1 и r2 как множеств (в результате
-- должен получиться список без повторений)
add :: R -> R -> R
add (R r1) (R r2) = R(nub $ r1 ++ r2)

-- rev r --- отношение, "обратное" к r (т.е. в каждой паре надо
-- компоненты поменять местами).
rev :: R -> R
rev (R r) = R $ map f r where
			f (x,y) = (y,x)

-- join r1 r2 --- отношение, состоящее из тех и только тех пар
-- (x, y), для которых существует z, такое, что (x, z) принадлежит
-- r1 и (z, y) принадлежит r2
join :: R -> R -> R
join (R r1) (R r2) = R([(x,y) | (x,z) <- r1, (zind,y) <- r2, zind == z])

-- closure r строит транзитивное замыкание отношения r (т.е. наименьшее
-- транзитивное отношение, содержащее r)
closure :: R -> R
closure = R . closure' . unR where
    closure' s = if s == s' then s else closure' s' where
        s' = nub $ s ++ ([(x,y) | (x,z) <- s, (zind,y) <- s, zind == z])

-- isReflexive r == True <=> r --- рефлексивное отношение на dom r
isReflexive :: R -> Bool
isReflexive r@(R s) = [(x,x) | x <- dom r, (x,x) `elem` s] == s

-- isSymmetric r == True <=> r --- симметричное отношение
isSymmetric :: R -> Bool
isSymmetric (R r) = [(a,b) | a <- dom (R r), b <- dom (R r), (a,b) `elem` r, (b,a) `elem` r] == r

-- isTransitive r == True <=> r --- транзитивное отношение
isTransitive :: R -> Bool
isTransitive r = r == closure r

-- isEquivalence r == True <=> r --- отношение эквивалентности
isEquivalence :: R -> Bool
isEquivalence r = if (False `elem` [isReflexive r, isSymmetric r, isTransitive r]) then False else True