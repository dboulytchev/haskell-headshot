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
fromFun f m n = G (fromF f m n) where
    fromF f m n | (m == n)  = [(m, f m)]
                | (m < n) = (m, f m): (fromF f (m + 1) n)  
                | otherwise = []

-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun (G a) = to a where
        to (a:as) b = 
            let (x1, y1) = a
                in if (x1 == b) then y1 
                    else to as b
        to [] b = b        
-- Графики можно сравнивать на равенство
instance Eq Graph where
  (==) (G a) (G b) = compare' a b 1 where
      compare' [] [] _ = True
      compare' [] b 0 = True
      compare' [] b 1 = False
      compare' a [] 1 = False
      compare' (a:as) b _ = let res = [x | x <- b, a == x] 
                              in if length res > 0 then compare' as b 0 else False              
      

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  (<=) (G a) (G b) | length a <= length b = True 
                   | otherwise = False

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom (G []) = []
dom (G (x:xs)) = d (x:xs) where
    d [] = []
    d (x:xs) = let (a, b) = x in a : (d xs)
    
-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose (G as) (G bs) = G (super as bs) where
    super [] [] = [] 
    super [] _ = [] 
    super _ [] = [] 
    super (a:as) (b:bs) = 
        let (x1, y1) = a
            (x2, y2) = b
            in if (y1 == x2) then (x1, y2) : (super as (b:bs))
                else super (a:as) bs    
     
-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.
restrict :: Graph -> [Int] -> Graph
restrict (G a) l = G (subset a l) where
    subset _ [] = []
    subset [] list = []
    subset (a:as) (l:ls) = 
        let (x, y) = a
          in   if (x == l) then a : subset as (l:ls) else subset (a:as) ls            
                   

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing (G (a:b:bs)) =
    let (x1, y1) = a
        (x2, y2) = b
    in if ((y1 <= y2) && (x1 <= x2)) || ((y1 >= y2) && (x1 >= x2)) then isIncreasing (G (b:bs)) else False
isIncreasing (G [x]) = True
isIncreasing (G [])  = True    

-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective (G []) = True
isInjective (G a) = 
    let res = [(x, y) | (x, y) <- a, (x', y') <- a, x' == x] in
        if length res > 0 then False else True

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse (G []) (G []) = True
areMutuallyInverse (G a) (G (b:bs)) = (==) (G a) (G (invertB (b:bs))) where
    invertB [] = []
    invertB (b:bs) = let (x, y) = b in (y, x) : invertB bs
