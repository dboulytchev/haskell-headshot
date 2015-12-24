module While where

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
instance Eq E where
    C x == C y = x == y
    _ == _ = False
-- Пример: факториал
fact = READ "n" :>>:
       "f" ::=: C 1 :>>:
       WHILE (X "n" :>: C 0) 
         ("f" ::=: X "f" :*: X "n" :>>:
          "n" ::=: X "n" :-: C 1 
         )  :>>:
       WRITE (X "f")
test = READ "n" :>>:
       "n" ::=: X "n" :>>:
     --  WHILE (X "n" :>: C 0) ("n" ::=: C (-1)) :>>:
       WRITE (X "n")
-- Написать интерпретатор int, который получает программу и
-- входной поток в виде списка целых, и возвращает результат: либо 
-- сообщение об ошибке, либо выходной поток в виде списка целых.
-- Например: 
--  int fact [5] => Right 120
--  int fact []  => Left "empty input"
int :: S -> [Int] -> Either String [Int] 
int prog inp = do
    ((_, res), _) <- inter prog inp (\ _ -> Nothing) []
    return res

inter :: S -> [Int] -> (String -> Maybe E) -> [Int] -> Either String (([Int], [Int]), (String -> Maybe E))
inter SKIP inp fstate res = Right ((inp, res), fstate)
inter (var ::=: val) inp fstate res = helper (eval val fstate) where
    helper (Left m) = Left m
    helper x = Right ((inp, res), (\a -> if (a == var) then Just (C val'') else fstate a))
        where (Right val'') = x
inter (com1 :>>: com2) inp fstate res = do
    ((inp', out), fs') <- inter com1 inp fstate res
    res' <- inter com2 inp' fs' out
    return res'
inter (IF cond com1 com2) inp fstate res = do
        cur <- temp
        return cur
    where
        temp = do
            evalled <- eval cond fstate 
            if (evalled == 1) then inter com1 inp fstate res else inter com2 inp fstate res
inter (READ var) (h : t) fstate res = Right ((t, res), (\a -> if (a == var) then Just (C h) else fstate a))
inter (WRITE smth) inp fstate res = do 
            a <- eval smth fstate
            return ((inp, res ++ [a]), fstate)
inter (WHILE cond com) inp fstate res = do
    cond' <- eval cond fstate
    ((inp', out), fs') <- inter com inp fstate res
    if (cond' == 1) then 
            inter (WHILE cond com) inp' fs' (res ++ out)
                else if (cond' == 0) then return ((inp, res), fstate)
                    else Left "Error while evaluating in `while`"
    
eval :: E -> (String -> Maybe E) -> Either String Int
eval (X var) fstate = if (res == Nothing) then Left "Unknown variable" else res''
    where
        res = fstate var
        (Just res') = res
        res'' = eval res' fstate
eval (C con) _ = Right con
eval (a :+: b) f = do
    a' <- eval a f
    b' <- eval b f
    return (a' + b')
eval (a :-: b) f = do
    a' <- eval a f
    b' <- eval b f
    return (a' - b')
eval (a :*: b) f = do
    a' <- eval a f
    b' <- eval b f
    return (a' * b')
eval (a :/: b) f = do
    a' <- eval a f
    b' <- eval b f
    if (b' == 0) then Left "Division by zero"
    else return (a' `div` b')
eval (a :%: b) f = do
    a' <- eval a f
    b' <- eval b f
    if (b' == 0) then Left "Division by zero"
    else return (a' `mod` b')
eval (a :=: b) f = do
    a' <- eval a f
    b' <- eval b f
    return (if (a' == b') then 1 else 0)
eval (a :/=: b) f = do
    a' <- eval a f
    b' <- eval b f
    return (if (a' /= b') then 1 else 0)
eval (a :<: b) f = do
    a' <- eval a f
    b' <- eval b f
    return (if (a' < b') then 1 else 0)
eval (a :>: b) f = do
    a' <- eval a f
    b' <- eval b f
    return (if (a' > b') then 1 else 0)
eval (a :/\: b) f = do
    a' <- eval a f
    b' <- eval b f
    if ((a' /= 1) && (a' /= 0) || ((b' /= 1) && (b' /= 0))) 
        then Left "Illegal values (not 0 or 1)"
            else return (if (a' == 0 || b' == 0) then 0 else 1)
eval (a :\/: b) f = do
    a' <- eval a f
    b' <- eval b f
    if ((a' /= 1) && (a' /= 0) || ((b' /= 1) && (b' /= 0))) 
        then Left "Illegal values (not 0 or 1)" 
            else return (if (a' == 0 && b' == 0) then 0 else 1)
{-E :+:  E | -- сложение
         E :-:  E | -- вычитание
         E :*:  E | -- умножение
         E :/:  E | -- частное
         E :%:  E | -- остаток
         E :=:  E | -- сравнение на "равно"
         E :/=: E | -- сравнение на "не равно"
         E :<:  E | -- сравнение на "меньше"
         E :>:  E | -- сравнение на "больше"
         E :/\: E | -- конъюнкция
         E :\/: E   -- дизъюнкция-}
-- Написать на While проверку простоты числа isPrime. Например,
--   int isPrime [5] => Right 1
--   int isPrime [8] => Right 0
isPrime = 
    READ "x" :>>:
    "res" ::=: C 1 :>>:
    "i" ::=: ((X "x" :/: C 2) :+: C 1) :>>:
    WHILE (X "i" :>: C 1) (
        IF (X "x" :%: X "i" :=: C 0) ("res" ::=: (C 0)) SKIP :>>:
         "i" ::=: (X "i" :-: C 1)
    ) :>>:
    WRITE (X "res")
 {-
inter (var ::=: val) inp fstate res = if (val' == Left "Unknown variable") then Left "Unknown variable" else Right ((inp, res), (\a -> if (a == var) then Just val'' else fstate a))
-}