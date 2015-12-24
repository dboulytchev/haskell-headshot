module Graph where

-- График целочисленной функции f --- это список пар (x, y), такой, что 
-- f (x) = y <=> пара (x, y) входит в этот список. Гарантируется, что
-- в списке нет двух пар с одинаковыми первыми компонентами; никаких
-- предположений о порядке следования пар в списке не делается. 
-- Далее везде считается, что все графики конечны.
newtype Graph = G {unG :: [(Int, Int)]} deriving Show

-- fromFun f m n строит график функции f в области определения от
-- m до n включительно c шагом 1.
fromFun :: (Int -> Int) -> Int -> Int -> Graph
fromFun = undefined

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun = undefined

-- Графики можно сравнивать на равенство
instance Eq Graph where
  (==) = undefined

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  (<=) = undefined

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom = undefined

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose = undefined
  
-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict = undefined

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing = undefined

-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective = undefined

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse = undefined