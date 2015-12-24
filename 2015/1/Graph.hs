module Graph where

import Data.List

import Data.Maybe

-- График целочисленной функции f --- это список пар (x, y), такой, что 
-- f (x) = y <=> пара (x, y) входит в этот список. Гарантируется, что
-- в списке нет двух пар с одинаковыми первыми компонентами; никаких
-- предположений о порядке следования пар в списке не делается. 
-- Далее везде считается, что все графики конечны.
newtype Graph = G {unG :: [(Int, Int)]} deriving Show

-- fromFun f m n строит график функции f в области определения от
-- m до n включительно c шагом 1.
fromFun :: (Int -> Int) -> Int -> Int -> Graph
fromFun f a b = G (zip r (map f r)) where r = [a..b]

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun gr x = case gr of
                    G lst -> case find (\(a,b) -> a==x) lst of
                                Just (v, u) -> u
                                Nothing -> 0

-- Графики можно сравнивать на равенство
instance Eq Graph where
  (==) g1 g2 = case g1 of
                G s -> case g2 of
                    G t -> (sb s) == (sb t)
                        where sb = sortBy (\a b -> compare (fst a) (fst b))

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  (<=) g1 g2 = foldl (&&) True [elem x (unG g2) | x <- (unG g1)]

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom g = case g of G lst -> map fst lst

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose _ (G []) = G []
compose g1 g2 = G [(fst x, toFun g2 $ snd x) | x <- unG g1]
  
-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict g l = case g of
        G lst -> G (filter
                    (\(u,v) -> 
                        case find (\x -> u==x) l of
                            Just _ -> True
                            Nothing -> False
                    ) 
                    lst)

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing g = sort (unG g) == s
                 where s = sortBy (\a b -> compare (snd a) (snd b)) (unG g)

-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective g = rv == nub rv
                where rv = (map (\(u,v)->v) (unG g))

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse g1 g2 = case g2 of G t -> g1 == G (map (\(u,v)->(v,u)) t)
