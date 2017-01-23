module Parser where

import Data.List as List

data Command = Exit | Read | Jump deriving Show

data Inst = InstExit { label :: Maybe String, com :: Command} | InstRead { label :: Maybe String, com :: Command} | InstJump { label :: Maybe String, com :: Command, num :: Int, goTo :: String} deriving Show

start text = makeListOfInst $ lines text

makeListOfInst [] = []
makeListOfInst (s:xs) = (getInst $ words s) : makeListOfInst xs


getInst inst@(l : xs) = case isLable l of
                   Just a   -> getCom (Just a) xs
                   Nothing  -> getCom Nothing inst

isLable s | last s == ':' = Just $ init s
          | otherwise     = Nothing

getCom lab inst@(c : xs) | c == "e"  = (InstExit lab Exit)
                         | c == "r"  = (InstRead lab Read)
                         | otherwise = getJump lab inst

getJump lab [j,n,l] = (InstJump lab Jump (read n :: Int) l)
