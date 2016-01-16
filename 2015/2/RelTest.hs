import Data.List
import Tester
import Rel

rel :: Int -> IO ([Int], R)
rel n = do
  dom <- distinct topValue n
  idx <- distinct n n
  return (dom, R (zip dom (map ((dom!!) . (+ (negate 1))) idx)))

tests :: [IO ()]
tests = [
         test "(==)" (do
                        putStr "Testing (R []) == (R []) ... "
                        assertResult (==) True $ (R []) == (R [])
                        (_, r) <- rel 5
                        putStr $ "Testing (R []) == " ++ show r ++ " ... "
                        assertResult (==) False $ (R []) == r
                        putStr $ "Testing " ++ show r ++ " == (R []) ... "
                        assertResult (==) False $ r == (R [])
                        let pr = map R $ (permutations . unR) r
                        many $
                          map (\ p -> do 
                                putStr $ "Testing " ++ show r ++ " == " ++ show p ++ " ... "
                                assertResult (==) True $ r == p
                              ) 
                              pr
                     ),
          test "dom" (do 
                        putStr "Testing dom (R []) ... "
                        assertResult (==) [] $ dom (R [])
                        (d, r) <- rel 10
                        putStr $ "Testing dom " ++ show r ++ " ... "
                        assertResult (\ x y -> sort x == sort y) d $ dom r
                     ),
          test "sub" (do 
                        putStr "Testing sub (R []) (R []) ... "
                        assertResult (==) True $ sub (R []) (R [])
                        (_, r) <- rel 15
                        putStr $ "Testing sub (R []) " ++ show r ++ " ... "
                        assertResult (==) True $ sub (R []) r
                        putStr $ "Testing sub " ++ show r ++ " (R []) ... "
                        assertResult (==) False $ sub r (R [])
                        putStr $ "Testing sub " ++ show r ++ " " ++ show r ++ " ... "
                        assertResult (==) True $ sub r r
                        let r' = R $ take 5 (unR r) 
                        putStr $ "Testing sub " ++ show r' ++ " " ++ show r ++ " ... "
                        assertResult (==) True $ sub r' r
                        putStr $ "Testing sub " ++ show r ++ " " ++ show r' ++ " ... "
                        assertResult (==) False $ sub r r'
                        let r'' = R . take 5 . reverse . unR $ r
                        putStr $ "Testing sub " ++ show r' ++ " " ++ show r'' ++ " ... "
                        assertResult (==) False $ sub r' r''
                        putStr $ "Testing sub " ++ show r'' ++ " " ++ show r' ++ " ... "
                        assertResult (==) False $ sub r'' r'                        
                     ),
          test "add" (do
                        putStr "Testing add (R []) (R []) ... "                      
                        assertResult (==) (R []) $ add (R []) (R [])
                        (_, r) <- rel 15
                        let r'  = R . take 7 . unR $ r
                        let r'' = R . take 8 . reverse . unR $ r
                        putStr $ "Testing add (R []) " ++ show r ++ " ... "
                        assertResult (==) r $ add (R []) r
                        putStr $ "Testing add " ++ show r ++ " (R []) ... "
                        assertResult (==) r $ add r (R [])
                        putStr $ "Testing add " ++ show r' ++ " " ++ show r'' ++ " ... "
                        assertResult (==) r $ add r' r''
                        putStr $ "Testing add " ++ show r'' ++ " " ++ show r' ++ " ... "
                        assertResult (==) r $ add r'' r'
                     ),
          test "rev" (do
                        putStr "Testing rev (R []))..."
                        assertResult (==) (R []) $ rev (R [])
                        (_, r) <- rel 15
                        putStr $ "Testing rev (rev " ++ show r ++ ") ... "
                        assertResult (==) r $ (rev . rev) r
                     ),
          test "join" (do
                         putStr "Testing join (R []) (R []) ... "
                         assertResult (==) (R []) $ join (R []) (R [])
                         (_, r) <- rel 15
                         m <- distinct topValue 15
                         let r'  = (R . (flip zip) m) (map fst $ unR r) 
                         let r'' = (R . zip m) (map snd $ unR r)
                         putStr $ "Testing join (R []) " ++ show r ++ " ... "
                         assertResult (==) (R []) $ join (R []) r
                         putStr $ "Testing join " ++ show r ++ " (R []) ... "
                         assertResult (==) (R []) $ join r (R [])
                         putStr $ "Testing join " ++ show r' ++ " " ++ show r'' ++ " ... "
                         assertResult (==) r $ join r' r''
                      ),
          test "closure/isTransitive" (do
                                         putStr "Testing closure (R []) ... "
                                         assertResult (==) (R []) $ closure (R [])
                                         putStr "Testing isTransitive (R []) ... "
                                         assertResult (==) True $ isTransitive (R [])
                                         (_, r) <- rel 15
                                         putStr $ "Testing isTransitive (closure " ++ show r ++ ") ... "
                                         assertResult (==) True $ isTransitive (closure r)
                                         d <- distinct topValue 10
                                         let r' = R $ zip d (tail d)
                                         putStr $ "Testing isTransitive " ++ show r' ++ " ... "
                                         assertResult (==) False $ isTransitive r'
                                         let r'' = R $ [(x, y) | x <- d, y <- d]
                                         putStr $ "Testing isTransitive " ++ show r'' ++ " ... "
                                         assertResult (==) True $ isTransitive r''        
                                         let r''' = R $ reverse (unR r')
                                         putStr $ "Testing isTransitive (closure " ++ show r''' ++ ") ... "
                                         assertResult (==) True $ isTransitive (closure r''')
                                      ),
           test "isReflexive" (do
                                 putStr "Testing isReflexive (R []) ... "
                                 assertResult (==) True $ isReflexive (R [])
                                 (d, r) <- rel 15
                                 let r' = R (nub $ unR r ++ [(x, x) | x <- d])
                                 putStr $ "Testing isReflexive " ++ show r' ++ " ... "
                                 assertResult (==) True $ isReflexive r'
                                 let r'' = R [(x, y) | (x, y) <- unR r', x /= y]
                                 putStr $ "Testing isReflexive " ++ show r'' ++ " ... "
                                 assertResult (==) False $ isReflexive r'' 
                              ),
           test "isSymmetric" (do
                                 putStr "Testing isSymmetric (R []) ... "
                                 assertResult (==) True $ isSymmetric (R [])
                                 (_, r) <- rel 15
                                 let r' = add r $ rev r
                                 putStr $ "Testing isSymmetric " ++ show r' ++ " ... "
                                 assertResult (==) True $ isSymmetric r'
                                 let r'' = (\ r' -> R $ foldl (\ acc (x, y) -> if elem (y, x) acc then acc else (x, y) : acc) [] r') $ unR r'
                                 putStr $ "Testing isSymmetric " ++ show r'' ++ " ... "
                                 assertResult (==) False $ isSymmetric r''
                              ),
           test "isEquivalence" (do
                                   putStr "Testing isEquivalence (R []) ... "
                                   assertResult (==) True $ isEquivalence (R [])
                                   d <- distinct topValue 20
                                   let c1 = take 10 d
                                   let c2 = take 10 $ reverse d
                                   let r1 = [(x, y) | x <- c1, y <- c1]
                                   let r2 = [(x, y) | x <- c2, y <- c2]
                                   let r  = R $ r1 ++ r2
                                   let r' = R $ take 50 r1
                                   putStr $ "Testing isEquivalence " ++ show r ++ " ... "
                                   assertResult (==) True $ isEquivalence r
                                   putStr $ "Testing isEquivalence " ++ show r' ++ " ... "
                                   assertResult (==) False $ isEquivalence r'
                                )
        ]

main = many tests
