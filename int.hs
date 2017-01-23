module Main where
import System.Environment
import qualified Parse
import Solver

main :: IO ()
main = do 
    args <- getArgs
    let fileName  = args !! 0
    let inputData = get $ tail args
    str <-  Parse.parse fileName
    case solve Nothing 0 inputData str str of 
        Nothing  -> putStrLn "."
        (Just a) -> putStrLn $ show a
    return ()

toInt s = read s :: Int

get :: [String] -> [Int]
get = map (toInt) 
