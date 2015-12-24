import While

result (Left s)    = error $ "Failed: " ++ s
result (Right [n]) = n

testList []     = ()
testList (f:fs) = f (testList fs)

main = do
  let a1 = testList [(\ _ -> let r = result (int isPrime [n]) in
                             let p = prime n in
                             if (r == 1 && p) || (r == 0) && not p then ()
                                                                   else error $ "Prime error: isPrime (" ++ show n ++ ") /= " ++ show p
                    ) | n <- [1..100]]
  let a2 = testList [(\ _ -> let r = result (int fact [n]) in
                             let p = fact' n in
                             if p == r then () 
                                       else error $ "Fact error: fact (" ++ show n ++ ") /= " ++ show p
                    )| n <- [1..8]]
  putStrLn $ show a1 ++ show a2  
  putStrLn "**********************"
  putStrLn "** All tests passed **"
  putStrLn "**********************"
  where
    fact' n = product [1..n]
    prime n = n <= 3 || iter 2 n 
    iter d n | d*d > n   = True
             | otherwise = n `rem` d /= 0 && iter (d+1) n
