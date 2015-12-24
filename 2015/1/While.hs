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

-- Написать интерпретатор int, который получает программу и
-- входной поток в виде списка целых, и возвращает результат: либо 
-- сообщение об ошибке, либо выходной поток в виде списка целых.
-- Например: 
--  int fact [5] => Right 120
--  int fact []  => Left "empty input"
int :: S -> [Int] -> Either String [Int]
int prog inp = 
    case interpret (make prog) (\_ -> Nothing) inp [] of
        Left error           -> Left error
        Right (_, _, _, out) -> Right out


type State = (String -> Maybe Int)

data Interpreter a = 
    Interpreter { interpret :: State -> [Int] -> [Int] -> Either String (a, State, [Int], [Int]) }

instance Functor Interpreter where
    fmap = liftM

instance Applicative Interpreter where
    pure x = Interpreter (\y inp out -> Right (x, y, inp, out))
    (<*>) = ap

instance Monad Interpreter where
    return = pure
    v >>= op = Interpreter $ \y inp out -> 
        case (interpret v y inp out) of
            Left str -> Left str
            Right (x, y', inp', out') -> interpret (op x) y' inp' out'

-- вычисляет значение из source и присваивает его в dest 
write :: String -> E -> Interpreter ()
write dest source = do 
    temp <- eval source
    Interpreter $ \y inp out -> 
        Right ((), \n -> 
            if n == dest then Just temp else y n, inp, out)

-- читает из входного потока
input :: Interpreter Int
input = Interpreter $ \y inp out -> 
    case inp of
        [] -> Left "Empty stream"
        (x:xs) -> Right (x, y, xs, out)

-- добавляет значение в выходной поток
output :: Int -> Interpreter ()
output x = Interpreter (\y inp out -> Right ((), y, inp, x:out))

-- выполняет команду языка
make :: S -> Interpreter ()
make SKIP = return ()
make (l ::=: r) = do write l r
make (READ n) = do 
    temp <- input
    write n (C temp)
make (WRITE expr) = do 
    temp <- eval expr
    output temp
make (s1 :>>: s2) = do 
    make s1
    make s2
make (IF expr s1 s2) = do 
    temp <- eval expr
    case temp of 
        1 -> make s1
        0 -> make s2
        otherwise -> undefined
make (WHILE expr s) = do 
    temp <- eval expr
    case temp of 
        1 -> make s >> make (WHILE expr s)
        0 -> return ()
        otherwise -> undefined


-- находит значение переменной
find :: String -> Interpreter Int
find x = Interpreter $ \y inp out -> 
    case y x of
        Nothing    -> Left "Incorect variable"
        Just value -> Right (value, y, inp, out)

boolToInt expr = if expr then 1 else 0

-- вычисляет значение выражения
eval :: E -> Interpreter Int
eval (X n) = do find n
eval (C x) = do return x
eval (l :+: r) = do 
    tempL <- eval l
    tempR <- eval r
    return (tempL + tempR)
eval (l :-: r) = do 
    tempL <- eval l
    tempR <- eval r
    return (tempL - tempR)
eval (l :*: r) = do 
    tempL <- eval l
    tempR <- eval r
    return (tempL * tempR)
eval (l :/: r) = do 
    tempL <- eval l
    tempR <- eval r
    return (tempL `div` tempR)
eval (l :%: r) = do 
    tempL <- eval l
    tempR <- eval r
    return (tempL `rem` tempR)
eval (l :=: r) = do
    tempL <- eval l
    tempR <- eval r
    return (boolToInt (tempL == tempR))
eval (l :/=: r) = do 
    tempL <- eval l
    tempR <- eval r
    return (boolToInt (tempL /= tempR))
eval (l :<: r) = do 
    tempL <- eval l
    tempR <- eval r
    return (boolToInt (tempL < tempR))
eval (l :>: r) = do 
    tempL <- eval l
    tempR <- eval r
    return (boolToInt (tempL > tempR))
eval (l :/\: r) = do 
    tempL <- eval l
    tempR <- eval r
    return $ if ((tempL `rem` 2) * (tempR `rem` 2) > 0) then 1 else 0
eval (l :\/: r) = do 
    tempL <- eval l
    tempR <- eval r
    return $ if ((tempL `rem` 2) + (tempR `rem` 2) > 0) then 1 else 0

-- Написать на While проверку простоты числа isPrime. Например,
--   int isPrime [5] => Right 1
--   int isPrime [8] => Right 0
isPrime =
    READ "x" :>>:
    "temp" ::=: C 2 :>>:
    "result" ::=: C 1 :>>:
    WHILE (X "temp" :<: X "x") (
        IF (X "x" :%: X "temp" :=: C 0) ("result" ::=: C 0) SKIP :>>:
        "temp" ::=: (X "temp" :+: C 1)
    ) :>>:
    WRITE (X "result")