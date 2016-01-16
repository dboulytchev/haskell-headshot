module Rel where

import Data.List
import Data.Tuple

-- Бинарное отношение на подмножестве целых чисел задано
-- списком пар без повторений
newtype R = R {unR :: [(Int, Int)]} deriving Show

-- Реализовать сравнение отношений на равенство (как множеств)
instance Eq R where
  (R r1) == (R r2) = (and [elem x r2|x <- r1]) && (and [elem y r1| y<-r2])

-- sub r1 r2 == True <=> r1 --- подмножество r2
sub :: R -> R -> Bool
sub (R r1) (R r2) = and [elem x r2| x <- r1]

-- dom r --- список (без повторений) целых чисел, каждое из
-- которых имеет хотя бы одно вхождение в r
dom :: R -> [Int]
dom (R r) = foldl (\acc x -> if not(elem x acc) then (x:acc) else acc)  [] r'
	where r' = foldl (\acc x -> (fst x):(snd x):acc) [] r

-- add r1 r2 --- объединение r1 и r2 как множеств (в результате
-- должен получиться список без повторений)
add :: R -> R -> R
add (R r1) (R r2) = R (foldl (\acc x -> if not(elem x acc) then (x:acc) else acc) r1 r2 )

-- rev r --- отношение, "обратное" к r (т.е. в каждой паре надо
-- компоненты поменять местами).
rev :: R -> R
rev (R r) = R(map (\x -> (snd x, fst x)) r)

-- join r1 r2 --- отношение, состоящее из тех и только тех пар
-- (x, y), для которых существует z, такое, что (x, z) принадлежит
-- r1 и (z, y) принадлежит r2
join :: R -> R -> R
join (R r1) (R r2) = R([(fst' x, snd' x)|x<-s])
	where 
		s = [(x,y,z1)| (x,z1)<- r1, (z2,y)<-r2, z1 == z2 ]
		fst' (x,_,_) = x
		snd' (_,y,_) = y
	
	
-- closure r строит транзитивное замыкание отношения r (т.е. наименьшее
-- транзитивное отношение, содержащее r)
closure :: R -> R
closure (R r) = add y (join y y)
	where y = R (foldl (\acc x -> if (elem x (unR(join (R r) (R r)))) then acc else x:acc) [] r)

-- isReflexive r == True <=> r --- рефлексивное отношение на dom r
isReflexive :: R -> Bool
isReflexive (R r) = and [elem (x,x) r| x <- dom (R r)]

-- isSymmetric r == True <=> r --- симметрияное отношение
isSymmetric :: R -> Bool
isSymmetric (R r) = and[elem (x,y) r|(y,x) <- r] 

-- isTransitive r == True <=> r --- транзитивное отношение
isTransitive :: R -> Bool
isTransitive (R r) = sub (closure (R r)) (R r)

-- isEquivalence r == True <=> r --- отношение эквивалентности
isEquivalence :: R -> Bool
isEquivalence (R r) = (isReflexive (R r))&&(isSymmetric (R r))&&(isTransitive (R r))
