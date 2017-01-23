module Main where
import System.Environment
import qualified Parse
import Solver

main :: IO ()
main = do 
    args <- getArgs
    let fileName  = args !! 0
    let inputData = get $ tail args
    putStrLn fileName
    putStrLn $ unwords $ map (show) inputData 
    let code = Parse.parse fileName
    str <- code
    putStrLn $ textToString str
    case solve Nothing 0 inputData str str of 
        Nothing  -> putStrLn "."
        (Just a) -> putStrLn $ show a
    return ()

textToString []     = []
textToString (x:xs) = helper x ++  textToString xs where
    helper Parse.R = "R " 
    helper Parse.E = "E "
    helper (Parse.J a b) = "J " ++ show a ++ " " ++ show b ++ " " 

toInt s = read s :: Int

get :: [String] -> [Int]
get = map (toInt) 
