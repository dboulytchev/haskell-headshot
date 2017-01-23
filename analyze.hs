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

take_jumps ((J a b):js) n acc = take_jumps js (n + 1) ((b, a, n):acc)
take_jumps j n acc = (j, acc)

to_blocks [] acc = acc
to_blocks ((J a b):cs) acc = to_blocks cs acc
to_blocks ((R x):cs) acc = to_blocks a ((x, b):acc) where (a, b) = take_jumps cs 0 []
to_blocks ((E x):cs) acc = to_blocks cs ((x, [(-2, -2, -2)]):acc)

sub_ways (a, b, c) (n, ls) = (add_to_list (n, b, c)) <$> filtered  where
                                                                         filtered = filter (\(z, x, y) -> z == a) ls
                                                                         add_to_list (n, q, c) (x, y, z) = (n, (q + y), (z:c))

ways p (-1, b, c) = [(-1, b, c)]
ways p (a, b, c) = concat ((sub_ways (a, b, c)) <$> p)

start_analyze [] acc = acc
start_analyze ((x, [(-2, -2, -2)]):cs) acc = start_analyze cs ((x, 0, []):acc)
start_analyze (c:cs) acc = start_analyze cs acc

find_min (x, y, z) [] = y
find_min (x, y, z) ((a, b, c):bs) = if (y > b) then find_min (a, b, c) bs else find_min (x, y, z) bs

reduce cs iter (filt:filts) = if (length filtered == 1) then (head filtered) else (analyze_programm cs filtered)
                              where min = find_min filt filts
                                    filtered = filter (\(a, b, c) -> b <= min) iter

analyze_programm cs set = if (null filtered_set) then (analyze_programm cs iter) else (reduce cs iter filtered_set)
                          where iter = concat ((ways cs) <$> set)
                                filtered_set = filter (\(a, b, c) -> a == (-1)) iter

main = do input <- getArgs
          file_a <- readFile (head input)
          let programm_a = parse <$> (split '\n' file_a)
          let blocks = to_blocks programm_a []
          let start = start_analyze blocks []
          let x = analyze_programm blocks start
          let (x, y, z) = analyze_programm blocks start
          putStrLn (show (y, z))
