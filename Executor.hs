module Executor where
import Parser

type Stream = [Int]
type Instruction = Int

data REGA = BOT | REGA Int
data REGD = REGD Int
data Context = Context REGA REGD Stream Instruction

execE :: REGA -> REGD -> Stream -> Maybe Int
execE BOT (REGD d) [] = Just d
execE _ _ _           = Nothing

execR :: REGA -> Stream -> Maybe (REGA, Stream)
execR BOT (x:xs) = Just (REGA x, xs)
execR _ _        = Nothing

execJ :: Context -> Int -> Instruction -> Maybe Context
execJ (Context (REGA 0) (REGD d) stream instr) n jmpInstr      = Just $ Context BOT (REGD $ d + n) stream jmpInstr
execJ (Context (REGA a) regd     stream instr) _ _ | a > 0     = Just $ Context (REGA $ a - 1) regd stream $ instr + 1
                                                   | otherwise = Nothing

evaluate :: Listing -> Context -> Maybe Int
evaluate listing ctx@(Context rega regd stream instr) = case listing !! instr of
    EXIT -> execE rega regd stream
    READ -> case execR rega stream of
        Just (newRega, newStream) -> evaluate listing (Context newRega regd newStream $ instr + 1)
        Nothing                   -> Nothing
    JMP acc (Label newAddr) -> case execJ ctx acc newAddr of
        Just newCtx -> evaluate listing newCtx
        Nothing     -> Nothing

initialContext :: Stream -> Context
initialContext stream = Context BOT (REGD 0) stream 0

exec :: Listing -> [String] -> IO ()
exec listing strStream = let
        stream :: Stream
        stream = map read strStream
    in case evaluate listing $ initialContext stream of
        Just n  -> print n
        Nothing -> putStrLn "."
