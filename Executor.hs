module Executor where
import Parser
import qualified Data.Map.Strict as Map

type Stream = [Int]
type REGD   = Int

data REGA = BOT | REGA Int

eval :: LabelMap -> [Command] -> REGA -> REGD -> Stream -> Maybe Int
eval _        (EXIT:_)          BOT      regd []     = Just regd
eval labelMap (READ:cs)         BOT      regd (x:xs) = eval labelMap cs (REGA x) regd xs
eval labelMap (JMP acc label:_) (REGA 0) regd stream = eval labelMap (labelMap Map.! label) BOT (regd + acc) stream
eval labelMap (JMP _ _:cs)      (REGA a) regd stream = eval labelMap cs (REGA $ a - 1) regd stream
eval _ _ _ _ _                                       = Nothing

exec :: LabelMap -> [Command] -> Stream -> IO ()
exec labelMap listing stream =
    case eval labelMap listing BOT 0 stream of
        Just n  -> print n
        Nothing -> putStrLn "."
