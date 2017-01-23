module Main where

import Parser
import Exec
import System.Environment
import System.IO

-- main = do 
-- 	   args <- getArgs
-- 	   let path = args !! 0
-- 	   let input = tail args
-- 	   paersed <- Parser.parse path
-- 	   ex      <- Exec.result paersed input
-- 	   return ()
	                                        


main = result "test0002.p" ["1","0"]