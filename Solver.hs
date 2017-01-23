module Solver where
import Parse

-- solve a d inputCode instr
solve Nothing d [] (E:instr) allInstr = Just d
solve Nothing _ xs (E:instr) allInstr = Nothing
solve Nothing _ [] (R:instr) allInstr = Nothing
solve (Just _) _ _ (R:instr) allInstr = Nothing 
solve Nothing d (x:xs) (R:instr) allInstr = solve (Just x) d xs instr allInstr
solve (Just 0) d xs ((J n l):instr) allInstr = solve Nothing (d + n) xs (drop (l - 1) allInstr) allInstr
solve Nothing d xs ((J n l):instr) allInstr = Nothing 
solve (Just a) d xs ((J n l):instr) allInstr | a > 0 = solve (Just (a - 1)) d xs instr allInstr
                                             | a < 0 = Nothing
