module Rel where

import Data.List
import Data.Tuple

-- Бинарное отношение на подмножестве целых чисел задано
-- списком пар без повторений
newtype R = R {unR :: [(Int, Int)]} deriving Show

-- Реализовать сравнение отношений на равенство (как множеств)
instance Eq R where
  (==) (R {unR = a}) (R {unR = b}) = (sub (R {unR = a}) (R {unR = b})) && (sub (R {unR = b}) (R {unR = a})) 
  

-- sub r1 r2 == True <=> r1 --- подмножество r2
sub :: R -> R -> Bool
sub (R {unR = []}) (R {unR = a}) = True
sub (R {unR = x:xs}) (R {unR = a}) = (elem x a) && (sub (R {unR = xs}) (R {unR = a}))

-- dom r --- список (без повторений) целых чисел, каждое из
-- которых имеет хотя бы одно вхождение в r
dom :: R -> [Int]
allT [] = []
allT (x:xs) = [fst x, snd x] ++ (allT xs) 
dom R {unR = []} = [] 
dom R {unR = (x:xs)} | elem (fst x) (allT xs) && elem (snd x) (allT xs) = dom (R {unR = xs})
		     | elem (fst x) (allT xs) = (snd x):dom (R {unR = xs})
		     | elem (snd x) (allT xs) = (fst x):dom (R {unR = xs})
		     | fst x == snd x = (fst x):dom (R {unR = xs})
		     | otherwise = [fst x, snd x] ++ dom (R {unR = xs})

-- add r1 r2 --- объединение r1 и r2 как множеств (в результате
-- должен получиться список без повторений)
add :: R -> R -> R
add (R {unR = []}) (R {unR = a}) = R {unR = a}
add (R {unR = (x:xs)}) (R {unR = a}) | elem x a = add (R {unR = xs}) (R {unR = a})
				     | otherwise = add (R {unR = xs}) (R {unR = a ++ [x]})

-- rev r --- отношение, "обратное" к r (т.е. в каждой паре надо
-- компоненты поменять местами).
rev :: R -> R
revT [] = []
revT (x:xs) = [(snd x, fst x)] ++ revT xs
rev R {unR = a} = R {unR = revT a}

-- join r1 r2 --- отношение, состоящее из тех и только тех пар
-- (x, y), для которых существует z, такое, что (x, z) принадлежит
-- r1 и (z, y) принадлежит r2
join :: R -> R -> R
filT a [] = []
filT (y:ys) (x:xs) | fst x == snd y = [(fst y, snd x)] ++ (filT (y:ys) xs)
	           | otherwise = filT (y:ys) xs
join' (R {unR = []}) (R {unR = a}) = []
join' (R {unR = (x:xs)}) (R {unR = a}) = (filT (x:xs) a) ++ join' (R {unR = xs}) (R {unR = a})
join (R {unR = a}) (R {unR = b}) = R {unR = join' (R {unR = a}) (R {unR = b})}

-- closure r строит транзитивное замыкание отношения r (т.е. наименьшее
-- транзитивное отношение, содержащее r)
closure :: R -> R
closure R {unR = x} = add a (join a a)
	where a = R (foldl (\acc b -> if (elem b (join' (R {unR = x}) (R {unR = x}))) then acc else b:acc) [] x)

--closure R {unR = x} = R {unR = nub ( x ++ join' (R {unR = x}) (R {unR = x}) )}

-- isReflexive r == True <=> r --- рефлексивное отношение на dom r
isReflexive :: R -> Bool
isReflexive R {unR = x} = and [elem (a,a) x | a <- dom (R {unR = x})]

-- isSymmetric r == True <=> r --- симметричное отношение
isSymmetric :: R -> Bool
isSymmetric R {unR = x} = and [elem (b, a) x | (a, b) <- x]

-- isTransitive r == True <=> r --- транзитивное отношение
isTransitive :: R -> Bool
isTransitive R {unR = y} = sub (closure (R {unR = y})) (R {unR = y})
 				
-- isEquivalence r == True <=> r --- отношение эквивалентности
isEquivalence :: R -> Bool
isEquivalence R {unR = x} = (isReflexive (R {unR = x})) && (isSymmetric (R {unR = x})) && (isTransitive (R {unR = x}))
