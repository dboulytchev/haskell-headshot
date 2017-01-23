import System.IO  
import System.Environment 
import Data.List.Split


data Instruction = E | R | J Int String deriving Show

parseSrcCode :: String -> [(Maybe String, Instruction)]
parseSrcCode s = map interpriate (map (splitOn ":") (lines s)) where
        interpriate [l, c] = (Just l,  comm $ words c)
        interpriate [c] = (Nothing, comm $ words c)
        comm ["e"] = E
        comm ["r"] = R
        comm ["j", i, k] = J (read i :: Int) k  

findLabel :: String -> [(Maybe String, Instruction)] -> [Instruction]
findLabel s str@((l, c):ls) = if l == Just s then map snd str else findLabel s ls

iter Nothing d [] (E:_) _ = Just d
iter Nothing d (x:xs) (R:com) s = iter (Just x) d xs com s
iter (Just 0) d xs ((J x l):com) s = iter Nothing (d + x) xs (findLabel l s) s
iter (Just a) d xs ((J x l):com) s = iter (Just (a - 1)) d xs com s 
iter _ _ _ _ _ = Nothing

parseAns Nothing  = "."
parseAns (Just x) = show x

main = do 
        args <- getArgs
        allFile <- readFile (head args)
        let intArgs = map (\x -> read x :: Int) (tail args)
        let listOfInsts = parseSrcCode allFile
        let answer = iter Nothing 0 intArgs (map snd listOfInsts) listOfInsts
        putStrLn (parseAns answer)
