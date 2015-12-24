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
fromFun f m n = G { unG = [ (x, f x) | x <- [m..n] ] }

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun = undefined

-- Графики можно сравнивать на равенство
instance Eq Graph where
  (==) a b = unG a == unG b

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  (<=) a b = foldl (&&) True [elem x $ unG b | x <- unG a]

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom f = [x | (x, _) <- unG f]

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose _ (G []) = G []
compose graph1 graph2 = G [(fst x, (toFun graph2) $ snd x) | x <- unG graph1]
  
-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict graph l = G [(x, toFun graph x) | x <- l `intersect` fst (unzip $ unG graph)]

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing (G []) = True
isIncreasing graph = length (filter (\(y1, y2) -> y1 < y2) (zip ys $ tail ys)) == (length graph' - 1) where
  graph' = sort $ unG graph
  ys = snd $ unzip graph'
  
-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective graph = length (nub list) == length list where
  list = snd $ unzip $ unG graph

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse graph1 graph2 = sort (unG graph1) == sort [(y, x) | (x, y) <- unG graph2]