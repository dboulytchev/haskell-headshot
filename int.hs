import System.Environment (getArgs)
import Parser
import Executor

main :: IO ()
main = do
    (filename:stream) <- getArgs
    (labelMap, listing) <- parse filename
    exec labelMap listing $ map read stream
--     print $ list

