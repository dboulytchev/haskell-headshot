module Graph where

import Data.List
import System.Random

newtype Graph = G {unG :: [(Int, Int)]} deriving Show

fromFun :: (Int -> Int) -> Int -> Int -> Graph
fromFun f m n = G [(x, f x) | x <- [m..n]]

findFx x = find (\(e1, e2) -> e1 == x)

toFun :: Graph -> (Int -> Int)
toFun (G g) = (\x -> case find (\(e1, e2) -> e1 == x) g of
						Just (_, x2) -> x2)
	
minX (G g) = minimum [x | (x, y) <- g]
maxX (G g) = maximum [x | (x, y) <- g]

instance Eq (Graph) where
  (==) (G g1) (G g2) = sort g1 == sort g2
							
instance Ord Graph where
  (<=) (G []) (G []) = True
  (<=) (G g1) (G g2) = let i = nub [elem x g2 | x <- g1] in 
					(head i && length i == 1) 

dom :: Graph -> [Int]
dom (G g) = [x | (x, _) <- g]
			
compose :: Graph -> Graph -> Graph
compose (G g1) (G []) = G []
compose (G g1) (G g2) = (G [(x, toFun (G g2) y) | (x, y) <- g1, (find (\(e1, e2) -> e1 == y) g2) /= Nothing])

restrict :: Graph -> [Int] -> Graph
restrict (G g) l = G [(x, f x) | x <- l, (find (\(e1, e2) -> e1 == x) g) /= Nothing] where
		f x = case (find (\(e1, e2) -> e1 == x) g) of
			Just (a1, a2) -> a2

isIncreasing :: Graph -> Bool
isIncreasing (G g) = 
	let l = [y | (x, y) <- (sort g)] in
		all (<=0) (zipWith (-) l (tail l))
		
isInjective :: Graph -> Bool
isInjective (G g) = 
	let l = [y | (x, y) <- g] in
		length l == length (nub l)
		
areMutuallyInverse :: Graph -> Graph -> Bool
areMutuallyInverse (G g1) (G g2) = (sort g1) == sort [ (snd x, fst x) | x <- g2] 
