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
fromFun f m n = G $ fromFun' m where
    fromFun' m = if (m > n) then [] else (m, f m):fromFun' (m+1)

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun g = foldl add (\x -> undefined) (unG g) where
    add f (x, y) = \z -> if (z == x) then y else f z

-- Графики можно сравнивать на равенство
instance Eq Graph where
  g1 == g2 = foldl eq (length pairs == length (unG g1)) pairs where
    pairs = zip (sort $ unG g1) (sort $ unG g2)
    eq acc ((x1, y1), (x2, y2)) = acc && (x1 == x2) && (y1 == y2)

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  g1 <= g2 = foldl (\acc (x, y) -> acc && (x `elem` d2) && (f2 x == y)) True (unG g1) where
    f2 = toFun g2
    d2 = dom g2

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom g = foldr (\(x, _) -> (:) x) [] (unG g)

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose g1 g2 = G $ foldl add [] $ dom g1 where
    add gs x = if (y `elem` d2) then (x, f2 y):gs else gs where
        y = f1 x
    f1 = toFun g1
    f2 = toFun g2
    d2 = dom g2
  
-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict g l = G $ foldl add [] l where
    add gs x = if (x `elem` dg) then (x, f x):gs else gs
    f = toFun g
    dg = dom g

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing g = f sg where
    f []       = True
    f [_]      = True
    f ((_, y1):x@(_, y2):xs) = y1 <= y2 && f (x:xs)
    sg = sort $ unG g

-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective g = length vg == length (nub vg) where
    vg = foldr (\(_, y) -> (:) y) [] (unG g)

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse g = (==) $ inv g where
    inv g = G $ map sw $ unG g
    sw (x, y) = (y, x)
