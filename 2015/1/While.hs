module While where

import Control.Monad

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



type State  = (String -> Maybe Int)

-- State + input stream + output stream
data Interp a = Interp { run :: State -> [Int] -> [Int] -> Either String (a, State, [Int], [Int]) }

instance Functor Interp where
    fmap = liftM
instance Applicative Interp where
    pure x = Interp $ \s si so -> Right (x, s, si, so)
    (<*>) = ap
instance Monad Interp where
    return = pure
    i >>= op = Interp $ \s si so -> case run i s si so of
                                        Left str      -> Left str
                                        Right (x, s', si', so') -> run (op x) s' si' so'

-- Чтение переменной из состояния
rd :: String -> Interp Int
rd x = Interp $ \s si so -> case s x of
                                Nothing -> Left "Unbound variable"
                                Just v  -> Right (v, s, si, so)

-- Запись переменной в состояние
wr :: String -> E -> Interp ()
wr nn ne = do nv <- eval ne
              Interp $ \s si so -> Right ((), \n -> if n == nn then Just nv else s n, si, so)

-- Чтение значения из входного потока
cin :: Interp Int
cin = Interp $ \s si so -> case si of
                                [] -> Left "Input stream empty"
                                (x:si') -> Right (x, s, si', so)

-- Запись значения в выходной поток
cout :: Int -> Interp ()
cout v = Interp $ \s si so -> Right ((), s, si, v:so)

-- Вычисление выражения
eval :: E -> Interp Int
eval (C x) = do return x
eval (X n) = do rd n
eval (e1 :+: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       return (v1 + v2)
eval (e1 :-: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       return (v1 - v2)
eval (e1 :*: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       return (v1 * v2)
eval (e1 :/: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       if v2 == 0
                         then fail "Division by zero"
                         else return (v1 `div` v2)
eval (e1 :%: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       return (v1 `rem` v2)
eval (e1 :=: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       return $ if (v1 == v2) then 1 else 0
eval (e1 :/=: e2) = do v1 <- eval e1
                       v2 <- eval e2
                       return $ if (v1 /= v2) then 1 else 0
eval (e1 :<: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       return $ if (v1 < v2)  then 1 else 0
eval (e1 :>: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       return $ if (v1 > v2)  then 1 else 0
eval (e1 :/\: e2) = do v1 <- eval e1
                       v2 <- eval e2
                       if (v1 /= 0) || (v1 /= 1) || (v2 /= 0) || (v2 /= 1)
                         then fail "Incorrect argument for logical operation"
                         else return $ if (v1 * v2 > 0) then 1 else 0
eval (e1 :\/: e2) = do v1 <- eval e1
                       v2 <- eval e2
                       if (v1 /= 0) || (v1 /= 1) || (v2 /= 0) || (v2 /= 1)
                         then fail "Incorrect argument for logical operation"
                         else return $ if (v1 + v2 > 0) then 1 else 0

-- Выполняет statement
exec :: S -> Interp ()
exec SKIP         = return ()
exec (n ::=: v)   = do wr n v
exec (READ n)     = do v <- cin
                       wr n (C v)
exec (WRITE e)    = do v <- eval e
                       cout v
exec (s1 :>>: s2) = do exec s1
                       exec s2
exec (IF e s1 s2) = do v <- eval e
                       case v of 1 -> exec s1
                                 0 -> exec s2
                                 _ -> fail "Incorrect argument for IF"

exec w@(WHILE e s)  = do v <- eval e
                         case v of 1 -> exec s >> exec w
                                   0 -> return ()
                                   _ -> fail "Incorrect argument for WHILE"

-- Написать интерпретатор int, который получает программу и
-- входной поток в виде списка целых, и возвращает результат: либо 
-- сообщение об ошибке, либо выходной поток в виде списка целых.
-- Например: 
--  int fact [5] => Right 120
--  int fact []  => Left "empty input"
int :: S -> [Int] -> Either String [Int]
int p strIn = case run (exec p) (\_ -> Nothing) strIn [] of
                Left err          -> Left err
                Right (_, _, _, strOut) -> Right $ reverse strOut

-- Написать на While проверку простоты числа isPrime. Например,
--   int isPrime [5] => Right 1
--   int isPrime [8] => Right 0
isPrime =
    READ "n" :>>:
    "ans" ::=: C 1 :>>:
    "m" ::=: (X "n" :-: C 1) :>>:
    WHILE (X "m" :>: C 1) (
        IF (X "n" :%: X "m" :=: C 0) ("ans" ::=: (C 0)) SKIP :>>:
        "m" ::=: (X "m" :-: C 1)
    ) :>>:
    WRITE (X "ans")
