import Tester
import Rel
import EqClasses
import Data.List

rel :: Int -> IO ([Int], R)
rel n = do
  dom <- distinct topValue n
  idx <- distinct n n
  return (dom, R (zip dom (map ((dom!!) . (+ (negate 1))) idx)))

tests :: [IO ()]
tests = [
         test "equivalent classes" (do
                                 d <- distinct topValue 20
                                 let c1 = take 5 d
                                 let c2 = take 5 $ drop 5  d
                                 let c3 = take 5 $ drop 10 d
                                 let c4 = take 5 $ drop 15 d
                                 let r1 = [(x, y) | x <- c1, y <- c1]
                                 let r2 = [(x, y) | x <- c2, y <- c2]
                                 let r3 = [(x, y) | x <- c3, y <- c3]
                                 let r4 = [(x, y) | x <- c4, y <- c4]
                                 let r  = R $ r1 ++ r2 ++ r3 ++ r4
                                 let c  = classes r
                                 putStr "Testing the number of equivalence classes ... "
                                 assertResult (==) 4 $ length c
                                 putStr "Testing the contents of equivalence classes ... "
                                 assertResult (==) (sort $ map sort [c1, c2, c3, c4]) $ sort (map sort c)
                                )
        ]

main = many tests
