module Rel where

import Data.List
import Data.Tuple

-- Бинарное отношение на подмножестве целых чисел задано
-- списком пар без повторений
newtype R = R {unR :: [(Int, Int)]} deriving Show

-- Реализовать сравнение отношений на равенство (как множеств)
instance Eq R where
  (R r1) == (R r2) = (\ s -> s r1 == s r2) $ sortOn fst

-- sub r1 r2 == True <=> r1 --- подмножество r2
sub :: R -> R -> Bool
sub (R r1) (R r2) = and [x `elem` r2 | x <- r1]

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
rev (R r) = R $ map (\(x, y) -> (y, x)) r

-- join r1 r2 --- отношение, состоящее из тех и только тех пар
-- (x, y), для которых существует z, такое, что (x, z) принадлежит
-- r1 и (z, y) принадлежит r2
join :: R -> R -> R
join (R r1) (R r2) = R $ joinSets r1 r2

joinSets s1 s2 = [(x, y) | (x, z) <- s1, (z', y) <- s2, z == z']

-- closure r строит транзитивное замыкание отношения r (т.е. наименьшее
-- транзитивное отношение, содержащее r)
closure :: R -> R
closure = R . closureSets . unR where
    closureSets s = if s == s' then s else closureSets s' where
        s' = nub $ s ++ (joinSets s s)

-- isReflexive r == True <=> r --- рефлексивное отношение на dom r
isReflexive :: R -> Bool
isReflexive r@(R s) = and [(x, x) `elem` s | x <- dom r]

-- isSymmetric r == True <=> r --- симметричное отношение
isSymmetric :: R -> Bool
isSymmetric (R r) = and [(y, x) `elem` r | (x, y) <- r]

-- isTransitive r == True <=> r --- транзитивное отношение
isTransitive :: R -> Bool
isTransitive r = r == closure r

-- isEquivalence r == True <=> r --- отношение эквивалентности
isEquivalence :: R -> Bool
isEquivalence r = and [isReflexive r, isSymmetric r, isTransitive r]