module Parser where

import Data.List as List


data Inst = InstExit { label :: Maybe String} | InstRead { label :: Maybe String} | InstJump { label :: Maybe String, num :: Int, goTo :: String} deriving Show

start text = makeListOfInst $ lines text

makeListOfInst [] = []
makeListOfInst (s:xs) = (getInst $ words s) : makeListOfInst xs


getInst inst@(l : xs) = case isLable l of
                   Just a   -> getCom (Just a) xs
                   Nothing  -> getCom Nothing inst

isLable s | last s == ':' = Just $ init s
          | otherwise     = Nothing

getCom lab inst@(c : xs) | c == "e"  = (InstExit lab)
                         | c == "r"  = (InstRead lab)
                         | otherwise = getJump lab inst

getJump lab [j,n,l] = (InstJump lab (read n :: Int) l)
