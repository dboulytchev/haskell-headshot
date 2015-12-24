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
fromFun f m n = G [ (x, f x) | x <- [m..n] ]

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun g = 
    (\y -> find y (unG g)) where 
        find v (x:xs) = case (v == fst x) of
            True  -> snd x
            False -> find v xs

-- Графики можно сравнивать на равенство
instance Eq Graph where
  (==) f g = sort (unG f) == sort (unG g)

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  (<=) (G []) (G []) = True
  (<=) f g = 
      let res = nub [ elem x (unG g) | x <- (unG f)] in (length res == 1 && head res)

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom f = [ fst x | x <- (unG f)]

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose _ (G []) = G []
compose (G []) _ = G []
compose g1 g2 =
    let f = toFun g2 in (G [ (fst x, f (snd x)) | x <- unG g1])

-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict g l = G [ x | x <- (unG g), elem (fst x) l ]

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing g = 
    let listX = sort (unG g) in 
        let listY = [ snd x | x <- listX] in listY == sort listY

-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective g =
    let list = [ snd x | x <- (unG g)] in length (nub list) == length list

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse g1 g2 = 
    let temp = [ (snd x, fst x) | x <- (unG g2)] in (unG g1) == temp