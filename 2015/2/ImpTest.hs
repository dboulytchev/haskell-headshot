{-
import Tester
import Imp
import Data.List

tests :: [IO ()]
tests = [
         test "Imp/fact" (do
                            ns <- distinct 8 1 
                            let n = head ns 
                            putStr $ "Testing fact " ++ show n ++ " ... "
                            let (s, v) = fact n
                            assertResult (==) (foldl (*) 1 [1..n]) $ s undefined v
                         )
        ]

main = many tests
-}