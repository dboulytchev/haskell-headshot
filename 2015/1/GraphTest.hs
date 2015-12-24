import Data.List
import System.Random
import Graph

rand :: IO Int
--rand = print 100
rand = getStdRandom $ randomR (1, 1000)

some :: Int -> IO [Int]
some n | n > 20 = error $ "Non-observable test data requested: " ++ show n
some 0 = return []
some n = do
  x  <- rand
  xs <- some $ n-1
  return $ x:xs

distinct :: Int -> IO [Int]
distinct n | n > 20 = error $ "Non-observable test data requested: " ++ show n
distinct n = generate (return []) n where
  generate acc 0 = acc
  generate acc n = do
    acc' <- acc 
    k    <- rand
    if elem k acc' then generate acc n
                   else generate (return $ k:acc') (n-1)

graph :: Int -> IO Graph
graph n = do
  xs <- distinct n
  ys <- some n
  return $ G (zip xs ys)

ingraph :: Int -> IO Graph
ingraph n = do
  xs <- distinct n
  ys <- distinct n
  return $ G (zip xs ys)

testPermutations :: Eq a => (Graph -> a) -> Graph -> Maybe ((Graph, a), (Graph, a))
testPermutations f g = 
  let ps = map G (permutations $ unG g) in
  contradictory (zip ps $ map f ps) where
    contradictory (x@(_, xa):y@(_, ya):xs) | xa /= ya  = Just (x, y)
                                           | otherwise = contradictory (x:xs)
    contradictory _ = Nothing

rep :: Int -> (() -> IO ()) -> IO ()
rep 0 _ = return ()
rep n f = do
  _ <- f ()
  rep (n-1) f

main = do
  putStrLn "Smoke test..."
  putStrLn "============="
  let empty = G []
  if dom empty /= [] then error "Failed: dom (G []) /= []"
                     else putStrLn "  dom passed"
  if isIncreasing empty then putStrLn "  isIncreasing passed"
                        else error "Failed: isIncreasing (G []) /= True"
  if isInjective empty then putStrLn "  isInjective passed"
                       else error "Failed: isInjective (G []) /= True"
  if areMutuallyInverse empty empty then putStrLn "  areMutuallyInverse passed"
                                    else error "Failed: areMutuallyInverse (G []) (G []) /= True"
  if empty == empty then putStrLn "  (==) passed"
                    else error "Failed: (G []) /= (G [])"
  if empty <= empty then putStrLn "  (<=) passed"
                    else error "Failed: not (G []) <= (G [])"
  if restrict empty [1, 2, 3] == empty then putStrLn "  restrict passed"
                                       else error "Failed: restrict (G []) [1, 2, 3] /= (G [])"
  g <- graph 5
  if compose g empty == empty then putStrLn "  compose passed"
                              else error "Failed: compose ? empty /= empty"
  if compose empty g == empty then putStrLn "  compose passed"
                              else error "Failed: compose empty ? /= empty"

 {- putStrLn "Testing fromFun/toFun/==..."
  putStrLn "========================"
  rep 10 (\ _ -> do
            ys <- some 10
            let g = G $ zip [1..10] ys
            if (fromFun (toFun g) 1 10) /= g 
            then error $ "Failed: (fromFun (toFun g) 1 10 /= g) for g = " ++ show g
            else putStrLn "  passed"
         )
-}
  putStrLn "Testing ==/<=..."
  putStrLn "================"
  rep 10 (\ _ -> do
            g <- graph 5
            case testPermutations (==g) g of
              Nothing -> putStrLn "  passed"
              Just ((g1, a1), (g2, a2)) -> 
                 error $ "Failed: " ++ show g1 ++ " == " ++ show g ++ " => " ++ show a1 ++ ", but" ++
                         "          " ++ show g2 ++ " == " ++ show g ++ " => " ++ show a2
            case testPermutations (<=g) g of
              Nothing -> putStrLn "  passed"
              Just ((g1, a1), (g2, a2)) -> 
                 error $ "Failed: " ++ show g1 ++ " <= " ++ show g ++ " => " ++ show a1 ++ ", but" ++
                         "          " ++ show g2 ++ " <= " ++ show g ++ " => " ++ show a2
            g <- graph 10
            let g' = G (take 5 $ unG g)
            if g' <= g then putStrLn "  passed"
                       else error $ "Failed: " ++ show g' ++ " <= " ++ show g ++ " should be True"
            if g <= g' then error $ "Failed: " ++ show g ++ " <= " ++ show g' ++ " should be False"
                       else putStrLn "  passed"
         )  

  putStrLn "Testing dom..."
  putStrLn "=============="
  rep 10 (\ _ -> do
            g <- graph 5
            let d = sort . map fst . unG $ g
            if d == (sort $ dom g) then putStrLn "  passed"
                                   else error $ "Failed: dom (" ++ show g ++ ") /= " ++ show d
         )

  putStrLn "Testing compose/==..."
  putStrLn "====================="
  rep 10 (\ _ -> do
            xs <- distinct 10
            ys <- distinct 10
            zs <- distinct 10
            let g1    = G $ zip xs ys
            let g2    = G $ zip ys zs
            let g     = G $ zip xs zs
            let empty = G [] 
            if g == compose g1 g2 then putStrLn "  passed"
                                  else error $ "Failed: compose " ++ show g1 ++ " with " ++ show g2 ++ " /= " ++ show g
            if empty == compose g1 empty then putStrLn "  passed"
                                   else error $ "Failed: compose " ++ show g1 ++ " with " ++ show empty ++ " /= " ++ show empty
            if empty == compose empty g2 then putStrLn "  passed"
                                         else error $ "Failed: compose " ++ show empty ++ " with " ++ show g2 ++ " /= " ++ show empty
         )

  putStrLn "Testing restrict/==..."
  putStrLn "======================"
  rep 10 (\ _ -> do
            xs  <- distinct 20
            ys  <- distinct 20
            let xs' = take 10 xs
            let g1  = G $ zip xs  ys         
            let g2  = G $ zip xs' ys
            if g2 == restrict g1 xs' then putStrLn "  passed"
                                     else error $ "Failed: restrict " ++ show g1 ++ " to " ++ show xs' ++ " /= " ++ show g2
         )

  putStrLn "Testing isIncreasing..."
  putStrLn "======================="
  rep 10 (\ _ -> do
            xs <- distinct 5
            ys <- distinct 5
            let gi  = G $ zip (sort xs) (sort ys)
            let gni = G $ zip (sort xs) (reverse $ sort ys)
            if isIncreasing gi  then putStrLn "  passed"
                                else error $ "Failed: isIncreasing " ++ show gi ++ " /= True"
            if isIncreasing gni then error $ "Failed: isIncreasing " ++ show gi ++ " /= False" 
                                else putStrLn "  passed"
            case testPermutations isIncreasing gi of
              Nothing -> putStrLn "  passed"
              Just ((g1, a1), (g2, a2)) -> error $ "Failed: isIncreasing " ++ show g1 ++ " == " ++ show a1 ++ " but" ++
                                                   "          isIncreasing " ++ show g2 ++ " == " ++ show a2
            case testPermutations isIncreasing gni of
              Nothing -> putStrLn "  passed"
              Just ((g1, a1), (g2, a2)) -> error $ "Failed: isIncreasing " ++ show g1 ++ " == " ++ show a1 ++ " but" ++
                                                   "          isIncreasing " ++ show g2 ++ " == " ++ show a2              
         )

  putStrLn "Testing isInjective..."
  putStrLn "======================"
  rep 10 (\ _ -> do
            xs <- distinct 10
            ys <- distinct  5
            let gi  = G $ zip (take 5 xs) ys
            let gni = G $ zip xs (ys ++ ys)
            if isInjective gi  then putStrLn "  passed"
                               else error $ "Failed: isInjective " ++ show gi ++ " /= True"
            if isInjective gni then error $ "Failed: isInjective " ++ show gi ++ " /= False" 
                               else putStrLn "  passed"
            case testPermutations isInjective gi of
              Nothing -> putStrLn "  passed"
              Just ((g1, a1), (g2, a2)) -> do error $ "Failed: isInjective " ++ show g1 ++ " == " ++ show a1 ++ " but" ++
                                                      "          isInjective " ++ show g2 ++ " == " ++ show a2              
         )

  putStrLn "Testing areMutuallyInverse..."
  putStrLn "============================="
  rep 10 (\ _ -> do
            xs <- distinct 10
            ys <- distinct 10
            let g1 = G $ zip xs ys
            let g2 = G $ reverse (zip ys xs)
            if areMutuallyInverse g1 g2 then putStrLn "  passed"
                                        else error $ "Failed: areMutuallyInverse " ++ show g1 ++ " and " ++ show g2 ++ " /= True"
            if areMutuallyInverse g2 g1 then putStrLn "  passed"
                                        else error $ "Failed: areMutuallyInverse " ++ show g2 ++ " and " ++ show g1 ++ " /= True"
            let ys' = take 5 ys
            let g3 = G $ zip xs (ys' ++ ys')
            if areMutuallyInverse g3 g2 then error $ "Failed: areMutuallyInverse " ++ show g3 ++ " and " ++ show g2 ++ " /= False"
                                        else putStrLn "  passed"
            if areMutuallyInverse g2 g3 then error $ "Failed: areMutuallyInverse " ++ show g2 ++ " and " ++ show g3 ++ " /= False"
                                        else putStrLn "  passed"
         )
  putStrLn ""
  putStrLn "**********************"
  putStrLn "** All tests passed **"
  putStrLn "**********************"