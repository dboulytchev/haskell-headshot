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
int s i = do
    (_, o, _) <- loop (\ x -> Left $ "undefined variable " ++ x) [] i s
    return $ reverse o where
  loop s o i SKIP       = return (s, o, i)
  loop s o i (x ::=: e) = do e' <- eval s e 
                             return (subst s x e', o, i)
  loop s o []     (READ _)  = Left "empty input"
  loop s o (z:zs) (READ x)  = return (subst s x z, o, zs)
  loop s o i      (WRITE e) = do e' <- eval s e
                                 return (s, e':o, i)
  loop s o i   (s1 :>>: s2) = do (s', o', i') <- loop s o i s1
                                 loop s' o' i' s2
  loop s o i   (IF c s1 s2) = do e' <- eval s c
                                 case e' of
                                   0 -> loop s o i s2
                                   1 -> loop s o i s1
                                   n -> Left $ "not a boolean value: " ++ show n
  loop s o i   (WHILE c s1) = do e' <- eval s c
                                 case e' of
                                   0 -> return (s, o, i)
                                   1 -> loop s o i (s1 :>>: WHILE c s1)
                                   n -> Left $ "not a boolean value: " ++ show n
  subst s x e y = if x == y then Right e else s y
  eval :: (String -> Either String Int) -> E -> Either String Int
  eval s (X x) = s x
  eval s (C n) = return n
  eval s (a :+:  b) = (+) <$> eval s a <*> eval s b
  eval s (a :-:  b) = (-) <$> eval s a <*> eval s b
  eval s (a :*:  b) = (*) <$> eval s a <*> eval s b
  eval s (a :/:  b) = div <$> eval s a <*> eval s b
  eval s (a :%:  b) = rem <$> eval s a <*> eval s b
  eval s (a :=:  b) = (toBool (==)) <$> eval s a <*> eval s b 
  eval s (a :/=: b) = (toBool (/=)) <$> eval s a <*> eval s b 
  eval s (a :<:  b) = (toBool (<))  <$> eval s a <*> eval s b 
  eval s (a :>:  b) = (toBool (>))  <$> eval s a <*> eval s b 
  eval s (a :/\: b) = boolOp (&&) (eval s a) (eval s b)
  eval s (a :\/: b) = boolOp (||) (eval s a) (eval s b)
  toBool f x y = if f x y then 1 else 0
  asBool 0 = Right False
  asBool 1 = Right True
  asBool n = Left $ "not a boolean value: " ++ show n
  boolOp f x y = do
    x' <- x
    y' <- y 
    toBool f <$> asBool x' <*> asBool y'

-- Написать на While проверку простоты числа isPrime. Например,
--   int isPrime [5] => Right 1
--   int isPrime [8] => Right 0
isPrime =
  READ "n" :>>:
  IF (X "n" :<: C 4) 
     (WRITE (C 1))
     ("d" ::=: C 2 :>>:
      "cont" ::=: C 1 :>>:
      WHILE ((X "n" :>: X "d" :*: X "d" :\/: X "n" :=: X "d" :*: X "d") :/\: X "cont")
            (IF (X "n" :%: X "d" :=: C 0)
                ("cont" ::=: C 0)
                ("d"    ::=: X "d" :+: C 1)
            ) :>>:
      WRITE (X "cont")
     )  
