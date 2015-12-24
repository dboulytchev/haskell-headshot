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
fromFun f m n = 
   G (to_list f m n)
   
to_list :: (Int -> Int) -> Int -> Int ->  [(Int, Int)] 
to_list f m n = 
  if (m==n) then [(m, f m)] 
  else [(m, f m)] ++ (to_list f (m+1) n) 
  
-- toFun получает график и возвращает функцию. 
toFun :: Graph -> (Int -> Int)
toFun (G (list @ (x:xs))) = 
  (\a -> search a list) where 
  search value (h:t) = 
    if(value == fst h) then snd h
	else search value t
   
toFun1 :: Graph -> (Int -> Int)
toFun1 g = foldl add (\x -> undefined) (unG g) where
    add :: (Int -> Int) -> (Int, Int) -> (Int -> Int)
    add f (x, y) = \z -> if (z == x) then y else f z
	
-- Графики можно сравнивать на равенство
instance Eq Graph where
  (==) (G l1) (G l2) = l1 == l2

-- Графики упорядочены по теоретико-множественному включению
instance Ord Graph where
  (<=) (G []) (G []) = True
  (<=) f g = 
      let res = nub [ elem x (unG g) | x <- (unG f)] in (length res == 1 && head res) 

-- dom g возвращает область определения графика
dom :: Graph -> [Int]
dom (G l) = foldl (\acc x->  acc ++ [(fst x)]) [] l   
dom2 (G l) = foldl (\acc x->  acc ++ [(snd x)]) [] l   

-- compose g1 g2 возвращает график суперпозиции функций с графиками
-- g1 и g2 (сначала применяется g1, потом g2)
compose :: Graph -> Graph -> Graph
compose g1 g2 = fromFun ((toFun g1).(toFun g2)) h (get_ends list) where 
  get_ends (y:ys) = 
    if(ys == []) then y
    else get_ends ys                  
  list@(h:t) = dom g1
  
-- restrict g l строит сужение графика g на l. Не предполагается,
-- что l --- подмножество dom g.

restrict :: Graph -> [Int] -> Graph
restrict gr (x:xs) = 
  fromFun (toFun gr) (max h  x) (min (get_end xs) (get_end t)) where 
    get_end (y:ys) = 
      if(ys == []) then y
      else get_end ys
    (h:t) = dom gr  

-- isIncreasing g == True <=> g --- график (нестрого) возрастающей функции
isIncreasing :: Graph -> Bool
isIncreasing gr =  
  helper (dom2 gr) where 
    helper (x:xs) =
     if(xs == []) then True 
     else (x <= head xs) && (helper xs)	 
	
	
-- isInjective g == True <=> g --- график инъективной функции
isInjective :: Graph -> Bool
isInjective gr = 
  helper list where 
    helper l = length(nub l) == (length l)
    list = dom2 gr

-- areMutuallyInverse g1 g2 == True <=> g1 и g2 --- графики взаимно-обратных
-- функций
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse = undefined
