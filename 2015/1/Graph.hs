module Graph where

import Data.List

-- График целочисленной функции f --- это список пар (x, y), такой, что 
-- f (x) = y <=> пара (x, y) входит в этот список. Гарантируется, что
-- в списке нет двух пар с одинаковыми первыми компонентами; никаких
-- предположений о порядке следования пар в списке не делается. 
-- Далее везде считается, что все графики конечны.
newtype Graph = G {unG :: [(Int, Int)]} deriving Show

-- fromFun f m n строит график функции f в области определения от
-- m до n включительно c шагом 1.
fromFun :: (Int -> Int) -> Int -> Int -> Graph
fromFun f m n = G [(x, f x) | x <- [m..n] ]

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun (G a) = (\x -> fx x a) 

fx :: Int -> [(Int, Int)] -> Int
fx val (x:xs) = if val == fst x then snd x else fx val xs

-- Графики можно сравнивать на равенство
instance Eq Graph where
  G a == G b = sort a == sort b

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  G a <= G b = foldl (&&) True [elem x b | x <- a]

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom (G a) = [fst x | x <- a]

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose (G []) _ = G []
compose _ (G []) = G []
compose (G a) (G b) = if [elem (snd x) dX | x <- a] == [] then G []
		else
		G ([(fst y, f2 $ x) | x <- dX, y <- a, (snd y) == x]) where 
				dX = dom (G b)
				f2 = toFun (G b)
  
-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict (G a) n = G [x | x <- a, elem (fst x) n] 

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing (G a) = listY == sort listY where listY = [snd x | x <- sort a] 

-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective (G a) = length (nub chkList) == length chkList where 
						chkList = [snd x | x <- a]

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse (G a) (G b) = sort [(snd x, fst x) | x <- b] == sort a