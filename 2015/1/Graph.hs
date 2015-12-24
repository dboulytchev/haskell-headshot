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
fromFun f m n = G {unG = [(x, f x) | x <- [m..n]]} 

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun g = (\x -> f x (unG g))
	where f x (y:ys) 
		| x == fst y = snd y
		| otherwise  = f x ys
		
-- Графики можно сравнивать на равенство
instance Eq Graph where
  (==) g1 g2 = (sort $ unG g1) == (sort $ unG g2)

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  (<=) (G []) (G []) = True
  (<=) g1 g2 = (head answ && length answ == 1) 
							where answ = nub [elem x (unG g2) | x <- (unG g1)] 

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom g = fst $ unzip $ unG g 

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose _ (G[]) = G []
compose g1 g2 = G {unG = [(fst x, snd y) | x <- unG g1, y <- unG g2, snd x == fst y]}
  
-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict g l = G [ x | x <- unG g, elem (fst x) l]

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции

isIncreasing :: Graph -> Bool
isIncreasing g = isSorted $ snd $ unzip $ sort $ unG g

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = all (\(x, y) -> x <= y) $ zip xs (tail xs)

-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective g = (length $ snd $ unzip $ unG g) == (length $ nub $ snd $ unzip $ unG g)

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse g1 g2 = sort ([(snd x, fst x) | x <- (unG g2)]) == sort (unG g1)
