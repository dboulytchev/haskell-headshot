module Graph where

import Data.List
import System.Random

newtype Graph = G {unG :: [(Int, Int)]} deriving Show

fromFun :: (Int -> Int) -> Int -> Int -> Graph
fromFun f m n = G [(x, f x) | x <- [m..n]]

findFx x = find (\(e1, e2) -> e1 == x)

toFun :: Graph -> (Int -> Int)
toFun (G g) = (\x -> case find (\(e1, e2) -> e1 == x) g of
						Just (x1, x2) -> x2)
					
minX (G g) =  minimum [x | (x, y) <- g]
maxX (G g) = maximum [x | (x, y) <- g]

instance Eq (Graph) where
  (==) (G g1) (G g2) = 
	let min1 = minimum ([x | (x, y) <- g1] ++ [x | (x, y) <- g2]) in
			isEq (G g1) (G g2) min1 where
			isEq (G g1) (G g2) x | x > maximum ([x | (x, y) <- g1] ++ [x | (x, y) <- g2]) = True
			isEq (G g1) (G g2) x = 
							if ((find (\(e1, e2) -> e1 == x) g1) == Nothing) && ((find (\(e1, e2) -> e1 == x) g2) == Nothing) 
							then isEq (G g1) (G g2) (x + 1)
							else
								if (find (\(e1, e2) -> e1 == x) g2) == Nothing 
								then False
								else (findFx x g1 == findFx x g2) && (isEq (G g1) (G g2) (x + 1))
							
instance Ord Graph where
  (<=) (G g1) (G g2) = 
	let min1 = minimum ([x | (x, y) <- g1]) in
		---max1 = maximum [x | (x, y) <- g2] in
			isEq (G g1) (G g2) min1 where
			isEq (G g1) (G g2) x | x > maximum ([x | (x, y) <- g1]) = True
			isEq (G g1) (G g2) x = 
								if ((find (\(e1, e2) -> e1 == x) g1) == Nothing) && ((find (\(e1, e2) -> e1 == x) g2) == Nothing) 
								then isEq (G g1) (G g2) (x + 1)
								else
									if (find (\(e1, e2) -> e1 == x) g2) == Nothing 
									then False
									else (findFx x g1 == findFx x g2) && (isEq (G g1) (G g2) (x + 1))
								
dom :: Graph -> [Int]
dom (G g) = foldl (\acc (x, y) -> if (find (\(e1, e2) -> e1 == x) g) /= Nothing then x : acc else acc) [min1..max1] g where
	min1 = minX (G g)
	max1 = maxX (G g)
			
compose :: Graph -> Graph -> Graph
compose (G g1) (G g2) = G $ [(x, f (G g2) y)| (x, y) <- g1] where
	f (G g) y = case (find (\(e1, e2) -> e1 == y) g2) of
					Just (a1, a2) -> a2
					
restrict :: Graph -> [Int] -> Graph
restrict (G g) l = G [(x, f x) | x <- l, (find (\(e1, e2) -> e1 == x) g) /= Nothing] where
		f x = case (find (\(e1, e2) -> e1 == x) g) of
			Just (a1, a2) -> a2

isIncreasing :: Graph -> Bool
isIncreasing (G g) = 
	let l = [y | (x, y) <- (sort g)] in
		all (>=0) (zipWith (-) l (reverse l))
		
isInjective :: Graph -> Bool
isInjective (G g) = 
	let l = [y | (x, y) <- g] in
		length l == length (nub l)
		
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse (G g1) (G g2) = g1 == [ (snd x, fst x) | x <- g2]
