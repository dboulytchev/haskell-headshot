module While where

import qualified Data.Map as M

infixl 7 :*:, :/:, :%:
infixl 6 :+:, :-:
infix  5 :=:, :/=:, :<:, :>:
infixl 4 :/\:
infixl 3 :\/:

data E = X String | -- переменная
         C Int    | -- константа
         E :+:  E | -- сложение
         E :-:  E | -- вычитание
         E :*:  E | -- умножение
         E :/:  E | -- частное
         E :%:  E | -- остаток
         E :=:  E | -- сравнение на "равно"
         E :/=: E | -- сравнение на "не равно"
         E :<:  E | -- сравнение на "меньше"
         E :>:  E | -- сравнение на "больше"
         E :/\: E | -- конъюнкция
         E :\/: E   -- дизъюнкция
 deriving Show

-- Примечание: операции сравнения возвращают 1 (истина) или 0 (ложь), 
-- логические связки определены, только если оба их аргумента --- 1 или 0.

infixr 1 :>>:
infix  2 ::=:

data S = SKIP          | -- пустой оператор (ничего не делает)

         String ::=: E | -- присваивание; "X" ::=: e вычисляет выражение e и 
                         -- присваивает его значение в переменную "X"

         READ String   | -- чтение из входного потока; READ "X" читает очередное 
                         -- значение из входного потока и записывает его в
                         -- переменную "X"; не определено, если входной поток пуст;
                         -- прочитанное значение удаляется из входного потока

         WRITE E       | -- WRITE e записывает значение выражения e в конец выходного 
                         -- потока

         S :>>: S      | -- s1 :>>: s2 последовательно исполняет сначала s1, потом s2

         IF E S S      | -- IF e s1 s2 вычисляет значение e; если это значение равно 1,
                         -- то исполняется s1, если 0 --- то s2, иначе всё неопределено

         WHILE E S       -- WHILE e s повторяет s, пока значение e равно 1; если значение
                         -- e равно 0, то всё, если ни 0, ни 1 --- всё не определено
  deriving Show

-- Пример: факториал
fact = READ "n" :>>:
       "f" ::=: C 1 :>>:
       WHILE (X "n" :>: C 0) 
         ("f" ::=: X "f" :*: X "n" :>>:
          "n" ::=: X "n" :-: C 1 
         ) :>>:
       WRITE (X "f")

-- Написать интерпретатор int, который получает программу и
-- входной поток в виде списка целых, и возвращает результат: либо 
-- сообщение об ошибке, либо выходной поток в виде списка целых.
-- Например: 
--  int fact [5] => Right 120
--  int fact []  => Left "empty input"
type Value = Int
type Error = String

evalExpr :: M.Map String [Value] -> E -> Either Error [Value]
evalExpr _ (C c) = Right [c]
evalExpr env (X s) = case M.lookup s env of
    Nothing -> Left "Unassigned variable"
    Just x -> Right x


evalExpr env (e1 :+: e2) = eval (+) left right
  where
    left = evalExpr env e1
    right = evalExpr env e2 
evalExpr env (e1 :-: e2) = eval (-) left right
  where
    left = evalExpr env e1
    right = evalExpr env e2 
evalExpr env (e1 :*: e2) = eval (*) left right
  where
    left = evalExpr env e1
    right = evalExpr env e2 
evalExpr env (e1 :/: e2) = eval div left right
  where
    left = evalExpr env e1
    right = evalExpr env e2 
evalExpr env (e1 :%: e2) = eval mod left right
  where
    left = evalExpr env e1
    right = evalExpr env e2 
evalExpr env (e1 :=: e2) = eval (\x y -> if x == y then 1 else 0) left right
  where
    left = evalExpr env e1
    right = evalExpr env e2 
evalExpr env (e1 :/=: e2) = eval (\x y -> if x /= y then 1 else 0) left right
  where
    left = evalExpr env e1
    right = evalExpr env e2 
evalExpr env (e1 :<: e2) = eval (\x y -> if x < y then 1 else 0) left right
  where
    left = evalExpr env e1
    right = evalExpr env e2 
evalExpr env (e1 :>: e2) = eval (\x y -> if x > y then 1 else 0) left right
  where
    left = evalExpr env e1
    right = evalExpr env e2 
evalExpr env (e1 :/\: e2) = eval (*) left right
  where
    left = evalExpr env e1
    right = evalExpr env e2 
evalExpr env (e1 :\/: e2) = eval (\x y -> 1-(1-x)*(1-y)) left right
  where
    left = evalExpr env e1
    right = evalExpr env e2 

eval _ (Left x) _  = Left x
eval _ _ (Left x)  = Left x
eval x (Right (i1:_)) (Right (i2:_)) = Right $ [x i1 i2]

-- evalStatement принимает текущее значение переменных и statement и возвращает новое значение переменных после его выполнения.
evalStatement :: M.Map String [Value] -> S -> Either Error (M.Map String [Value])
evalStatement env (var ::=: e) = 
  case evalExpr env e of 
    Left errs -> Left errs                                          
    Right val -> Right $ M.insert var val env
        
evalStatement env (WHILE e s) = 
  case evalExpr env e of 
    Left errs -> Left errs
    Right [0] -> Right env
    Right [1] -> 
      case (evalStatement env s) of
        Left errs -> Left errs
        Right env' -> evalStatement env' (WHILE e s)              
    Right _ -> Left "Types Error"


evalStatement env SKIP = Right env

evalStatement env (READ sym) = case unjust $ M.lookup "Input" env of
    [] -> Left "Empty input"
    (x:xs) -> Right $ M.insert sym [x] $ M.adjust (\(x:xs) -> xs) "Input" env

evalStatement env (WRITE e) = case evalExpr env e of
    Left err -> Left err
    Right x -> Right $ M.adjust (\y -> y ++ x) "Output" env

evalStatement env (s :>>: xs) =
  case evalStatement env s of 
    Left errs -> Left errs
    Right env' -> evalStatement env' xs


int :: S -> [Int] -> Either String [Int] 
int program inp = case evalStatement state program of 
        Left x -> Left x
        Right s -> Right $ unjust $ M.lookup "Output" s
    where state = M.insert "Output" [] $ M.insert "Input" inp M.empty

unjust (Just x) = x

-- Написать на While проверку простоты числа isPrime. Например,
--   int isPrime [5] => Right 1
--   int isPrime [8] => Right 0
isPrime = READ "n" :>>:
       "p" ::=: C 1 :>>:
       "tmp" ::=: X "n" :-: C 1 :>>:
       WHILE (X "tmp" :>: C 1) 
         ("p" ::=: X "p" :/\: X "n" :%: X "tmp" :/=: C 0 :>>:
          "tmp" ::=: X "tmp" :-: C 1 
         ) :>>:
       WRITE (X "p")
