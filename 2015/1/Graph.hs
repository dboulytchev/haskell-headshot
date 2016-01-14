module Graph where

import Data.List 
import Data.Maybe
import Data.Tuple

-- График целочисленной функции f --- это список пар (x, y), такой, что 
-- f (x) = y <=> пара (x, y) входит в этот список. Гарантируется, что
-- в списке нет двух пар с одинаковыми первыми компонентами; никаких
-- предположений о порядке следования пар в списке не делается. 
-- Далее везде считается, что все графики конечны.
newtype Graph = G {unG :: [(Int, Int)]} deriving Show

-- fromFun f m n строит график функции f в области определения от
-- m до n включительно c шагом 1.
fromFun :: (Int -> Int) -> Int -> Int -> Graph
fromFun f m n = G [(x, f x) | x <- [m..n]]

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun = foldl (\ f (x, fx) y -> if y == x then fx else f y) undefined . unG

-- Графики можно сравнивать на равенство
instance Eq Graph where
  (G g1) == (G g2) = (\ s -> s g1 == s g2) $ sortOn fst

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  (G g1) <= (G g2) = and [ elem p g2 | p <- g1]

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom = map fst . unG

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose (G g1) (G g2) = G $ do (x1, y1) <- g1
                               case lookup y1 g2 of
                                 Just y2 -> return (x1, y2)
                                 Nothing -> []
  
-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict g l = compose (G $ zip l l) g

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing (G g) = (\ s -> s == sort s) $ map snd (sortOn fst g)

-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective (G g) = length g == length (nub $ map snd g)

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse g1 (G g2) = g1 == (G $ map swap g2)