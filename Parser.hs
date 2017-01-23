module Parser where

import Data.List as List

data Command = Exit Bool | Read Bool | Jump Bool deriving Show

data Inst = Inst { label :: Maybe String, com :: Command, num :: Maybe Int, goTo :: Maybe String} deriving Show

start text = makeListOfInst $ lines text

makeListOfInst [] = []
makeListOfInst (s:xs) = (getInst $ words s) : makeListOfInst xs


getInst inst@(l : xs) = case isLable l of
                   (Just a) -> getCom (Just a) xs
                   Nothing  -> getCom Nothing inst

isLable s | last s == ':' = Just $ init s
          | otherwise     = Nothing

getCom lab inst@(c : xs) | c == "e"  = (Inst lab (Exit True)  Nothing Nothing)
                         | c == "r"  = (Inst lab (Read True) Nothing Nothing)
                         | otherwise = getJump lab inst

getJump lab (j:n:l) = (Inst lab (Jump True) (Just (read n :: Int)) (Just $ head l))
