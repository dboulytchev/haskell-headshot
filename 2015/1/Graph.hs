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
fromFun f m n 
  | ( m > n ) = G []
  | (n == m) = G [(m, f m)]
  | otherwise = G $ (m, f m) : list
  where G list = fromFun f (m + 1) n 

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun (G g) = \x -> f g x
  where f [] a = error "Function undefined"
        f ((x, y) : xs) a = if ( x == a) then y else f xs a
  -- Графики можно сравнивать на равенство
instance Eq Graph where
  (==) x y = (x <= y) && (y <= x)

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  (<=) (G []) (G b) = True
  (<=) (G (x : xs)) (G b) = (x `elem`b) && (xs <= b)

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom (G graph)= foldr (\(x,y) acc -> x : acc) [] graph
   
-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose (G g1) gr2@(G g2) = G $ foldr f2 [] g1
   where f = toFun gr2
         f2 (x, y) list = if (null (inn g2 y)) then list else (x, f y) : list 
  
-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
inn [] a = []
inn ((x, y) : xs) a = if (x == a) then [(x, y)] else inn xs a  
restrict :: Graph -> [Int] -> Graph 
restrict g [] = G []
restrict gr@(G g) (x : xs) = G $ (inn g x) ++ result
  where 
    G result = restrict gr xs
quicksort [] = []  
quicksort ((x, y):xs) =   
          let smallerSorted = quicksort [(a, b) | (a, b) <- xs, a <= x]  
              biggerSorted = quicksort [(a, b) | (a, b) <- xs, a > x]  
          in  smallerSorted ++ [(x, y)] ++ biggerSorted 
-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing (G []) = True
isIncreasing (G graph) = helper y xs
   where
     ((x, y) : xs) = quicksort graph
     helper a [] = True
     helper a ((x, y) : xs) = (a <= y) && (helper y xs)
-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective (G []) = True
isInjective (G ((x, y) : xs))= (isOne y xs) && ( isInjective (G xs))
 where
  isOne a [] = True
  isOne a ((x,y) : xs) = if (a == y) then False else isOne a xs 

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse g1 g2 = (compose g1 g2) == (compose g2 g1)
