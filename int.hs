{-#LANGUAGE MultiWayIf, FlexibleContexts #-}
module Main where
import Data.List
import Data.Char
import System.Environment
import System.IO

data Command a = J a a | R a | E a deriving Show -- ?

split d [] = []
split d s = x : split d (drop 1 y)
            where (x, y) = span (/= d) s

strToInt x = foldl (\a b -> 10 * a + (digitToInt b)) 0 x

parse_command (x:xs) = if | head x == 'j' -> J (strToInt (xs !! 0)) (strToInt (tail (xs !! 1)))
                          | head x == 'r' -> R (-1)
                          | head x == 'e' -> E (-2)
                          | head x == 'l' -> sub_parse (xs) (strToInt (tail (init x)))
                       where sub_parse (a:as) b = if | head a == 'r' -> R b
                                                     | head a == 'e' -> E b
                                                     | head a == 'j' -> J (strToInt (as !! 0)) (strToInt (tail (as !! 1)))

parse :: [Char] -> Command Int
parse s = parse_command ((split '\t' s) >>= (\x -> split ' ' x))

drop_until [] y = (E (-1), [])
drop_until ((J x z):cs) y = drop_until cs y
drop_until ((R x):cs) y = if x == y then ((R x), cs) else drop_until cs y
drop_until ((E x):cs) y = if x == y then ((E x), cs) else drop_until cs y

execute (R x) c [] a d = putStrLn "."
execute (R x) (c:cs) (i:is) a d = if a == (-1) then (execute c (cs ++ [R x]) is i d) else putStrLn "."
execute (E x) c [] a d = if a == (-1) then putStrLn (show d) else putStrLn "."
execute (E x) c (i:is) a d = putStrLn "."
execute (J x y) c i (-1) d = putStrLn "."
execute (J x y) c i 0 d = (execute r (rs ++ [J x y] ++ c) i (-1) (d + x)) where
                                                              (r, rs) = drop_until c y
execute (J x y) (c:cs) i a d = execute c (cs ++ [J x y]) i (a - 1) d

main :: IO ()
main = do input <- getArgs --input <- readLn
          --let (file_name : stream) = split ' ' input
          file <- readFile (head input)
          let programm = parse <$> (split '\n' file)
          execute (head programm) (tail programm) (strToInt <$> (tail input)) (-1) 0
          return ()