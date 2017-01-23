import System.Environment (getArgs)
import Parser
import Executor

main :: IO ()
main = do
    (filename:stream) <- getArgs
    list <- parse filename
    exec list stream
--     print $ list

