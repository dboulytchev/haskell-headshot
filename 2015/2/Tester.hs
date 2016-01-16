module Tester where

import Data.List
import System.Random
import System.Exit

topValue :: Int
topValue = 1000

rand :: Int -> IO Int
rand n = getStdRandom $ randomR (1, n)

distinct :: Int -> Int -> IO [Int]
distinct k n | n > 20 = error $ "Non-observable test data requested: " ++ show n
distinct k n = generate (return []) n where
  generate acc 0 = acc
  generate acc n = do
    acc' <- acc 
    k    <- rand k
    if elem k acc' then generate acc n
                   else generate (return $ k:acc') (n-1)

rep :: Int -> IO () -> IO ()
rep 0 _ = return ()
rep n f = do
  _ <- f 
  rep (n-1) f

oops :: IO ()
oops = do
  putStrLn "***************"
  putStrLn "*** Failure ***"
  putStrLn "***************"
  die ""

test :: String -> IO () -> IO ()
test name f = do
  putStrLn header
  putStrLn $ "*** Testing " ++ name ++ " ***"
  putStrLn header
  rep 10 f
  putStrLn "***************"
  putStrLn "*** Success ***"
  putStrLn "***************" where
    header = take (16 + length name) $ repeat '*'

assertResult :: Show a => (a -> a -> Bool) -> a -> a -> IO ()
assertResult f x y =
  if f x y 
  then putStrLn "ok" 
  else do
        putStrLn ""
        putStrLn $ "Expected: " ++ show x ++ ", actual: " ++ show y
        oops

many :: [IO ()] -> IO ()
many = foldl (>>) (return ()) 

