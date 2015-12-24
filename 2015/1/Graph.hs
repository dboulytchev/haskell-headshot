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
fromFun f l r = G {unG = [(x, f x) | x <- [l..r]]}

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun g = tf $ unG g
tf (h@(x, y) : t) = (\a -> if a == x then y else tf t a)
tf [] = (\_ -> error "Such x doesn't belong to the graph")

-- Графики можно сравнивать на равенство
instance Eq Graph where
  (==) a b = sort (unG a) == (sort (unG b))

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  (<=) a b = foldl (&&) True [x `elem` b' | x <- a'] where
    b' = unG b
    a' = unG a

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom g = [x | (x, _) <- unG g]

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose (G g1) (G g2) = if (check g1y g2x) then G res else G []
    where
        findByX :: Int -> [(Int, Int)] -> Int
        findByX x (h@(x1, y1) : t) = if (x == x1) then y1 else (findByX x t)
        findByX _ [] = error "Such x doesn't belong to the graph"
        res :: [(Int, Int)]
        res = [(x1, findByX y1 g2) | (x1, y1) <- g1]
        check y1s x2s = foldl (&&) True [y `elem` x2s | y <- y1s]
        g1y = snd (unzip g1)
        g2x = fst (unzip g2)

  
-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict g ls = G (restrict' g ls)
    where
        restrict' :: Graph -> [Int] -> [(Int, Int)]
        restrict' g [] = []
        restrict' g (h : t) = if (h `elem` xs) then (h, gety h) : (restrict' g t) else restrict' g t
        g' = unG g
        xs = fst (unzip g')
        gety = toFun g
-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing (G []) = True
isIncreasing g = length (filter (\(y1, y2) -> y1 < y2) (zip ys $ tail ys)) == (length g' - 1)
    where
        g' = sort $ unG g
        ys = snd $ unzip g'
-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective g = length ys == (length (nub ys))
        where
            g' = unG g
            ys = snd (unzip g')
-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse (G g1) (G g2) = sort g1 == (sort (swap' g2))
    where
        swap' ((x, y) : t) = (y, x) : (swap' t)
        swap' [] = []