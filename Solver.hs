module Solver where
import Parse

solve :: (Ord a, Num a) => Maybe a -> Int -> [a] -> [Instr] -> [Instr] -> Maybe Int
solve Nothing d [] (E:_) _                           = Just d
solve Nothing d (x:xs) (R:instr) allInstr            = solve (Just x) d xs instr allInstr
solve (Just 0) d xs ((J n l):instr) allInstr         = solve Nothing (d + n) xs (drop (l - 1) allInstr) allInstr
solve (Just a) d xs ((J n l):instr) allInstr | a > 0 = solve (Just (a - 1)) d xs instr allInstr
solve _ _ _ _ _                                      = Nothing
