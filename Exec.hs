module Exec where
import Parser 
import Data.List


data RegA = RegA {getA::Int} | Step deriving (Show, Eq) -- Step = _|_
type Regs = (RegA, Int) -- (a,d)

-- data Com = E | R | J {numb ::Int, label :: String} deriving Show -- ?

-- data Line = Line {lab:: String, command :: Com} deriving Show -- ?
-- type Programm = [Line]


startExec :: Programm -> [Int] -> Maybe (Int, Regs, [Int])
startExec lines stream  =  execLine (Just(0, (Step, 0), stream)) 
                           where
                            allProg = lines 
                            labList :: [String]
                            labList = [lab l | l <- lines]  
                            execLine :: Maybe (Int, Regs, [Int]) -> Maybe (Int, Regs, [Int])
                            execLine Nothing = Nothing
                            execLine (Just (numOfLine, (a, d), str)) = do                                                         
                                                              case command (allProg !! numOfLine) of
                                                                E -> if a == Step && null str then Just (0, (a,d), str) else Nothing
                                                                R -> if a == Step && not (null str) then execLine (Just(numOfLine+1, (RegA (head str), d), tail str)) else Nothing
                                                                J n ln -> if getA a == 0 then case (ln `elemIndex` labList) of 
                                                                                                    Just lbl -> execLine (Just (lbl, (Step, d + n), str))
                            
                                                                     else if getA a > 0 then execLine (Just(numOfLine+1, (RegA (getA a - 1), d), str)) else Nothing



            
result :: Programm -> [String] -> IO()                            
result l s = do 
    case startExec l (map (\n -> read n::Int) s) of 
      Just(_,a,_) -> (putStrLn $ show $ snd a)
      otherwise -> putStrLn "."
    return()
