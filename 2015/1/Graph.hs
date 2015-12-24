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
fromFun f m n = G{unG = [(x, f x)| x <- [m..n]]}

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun G{unG = xs} = (\ x -> f x xs)
     where f x [] = error "Nope"
	   f x (y:ys) = if x == y then y else f x ys

-- Графики можно сравнивать на равенство
instance Eq Graph where
 G{unG = xs} == G{unG = ys} = 
          sort xs == sort ys
   
-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
   G{unG = xs} <= G{unG = ys} =
          notElem False [elem x ys | x <- xs]

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom G{unG = []} = []
dom G{unG = xs} = [fst x| x <- xs]

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose G{unG = xs} G{unG = ys} = G{unG = [ (fst x, snd y) | x <- xs, y <- ys, snd x == fst y]}

-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict G{unG = xs} ys = G{unG = [(y, snd x) | x <- xs, y <- ys, y == fst x ]}

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing G{unG = xs} = notElem False $ [ snd x <= snd y | x <- xs, y <- xs, fst x < fst y ]

-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective G{unG = xs} = notElem False $ [ snd x == snd y | x <- xs, y <- xs, fst x == fst y]

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse G{unG = xs} G{unG = ys} = notElem False $ [fst x == snd y | x <- xs, y <- ys, fst y == snd x]
