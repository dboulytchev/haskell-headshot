module Imp where

-- Реализовать набор комбинаторов для императивного программирования
-- в Haskell (денотационный интерпретатор shallow-DSL)

-- Состояние отображает имена переменных в целые числа
type State = String -> Int

-- Выражение --- это функция, которая по состоянию возвращает
-- значение этого выражения
type Expr  = State -> Int

-- Оператор --- это преобразователь состояния
type Stmt  = State -> State

-- Комбинаторы выражений:
-- литерал (целое число)
lit :: Int -> Expr
lit n = (\_ -> n) 

-- переменная
var :: String -> Expr
var str f = f str  

-- бинарные операции
infixl 3 &&& -- конъюнкция
infixl 3 ||| -- дизъюнкция
infix  4 <!  -- "меньше"
infix  4 >!  -- "больше"
infix  4 === -- "равно"
infix  4 =/= -- "не равно"
infixl 5 +!  -- сумма
infixl 5 -!  -- разность
infixl 6 *!  -- произведение

(&&&), (|||), (<!), (>!), (===), (=/=), (+!), (-!), (*!) :: Expr -> Expr -> Expr

(&&&) x y = (*) <$> x <*> y
(|||) x y = (+) <$> x <*> y
(<!)  x y = (toBool (<)) <$> x <*> y
(>!)  x y = (toBool (>)) <$> x <*> y
(===) x y = (toBool (==)) <$> x <*> y
(=/=) x y = (toBool (/=)) <$> x <*> y
(+!)  x y = (+) <$> x <*> y
(-!)  x y = (-) <$> x <*> y
(*!)  x y = (*) <$> x <*> y

toBool f x y = if f x y then 1 else 0


-- Операторы
-- присваивание
infix 2 <:=

(<:=) :: String -> Expr -> Stmt
(<:=) str expr = \state x -> if (x == str) then expr state else state x

-- последовательое исполнение
infixr 1 !>

(!>) :: Stmt -> Stmt -> Stmt
(!>) st1 st2 = st2 . st1 

-- ветвление (if-then-else)
branch :: Expr -> Stmt -> Stmt -> Stmt
branch expr st1 st2 f = if (expr f /= 0) then st1 f else st2 f 
                  
-- цикл с предусловием                  
while :: Expr -> Stmt -> Stmt
while expr st f = if (expr f == 0) then f else while expr st (st f) 

-- Примеры:
-- выражение "a + b"
a_plus_b :: Expr
a_plus_b = var "a" +! var "b"

-- a + b в состоянии [a = 1, b = 3] вычисляется в
-- значение 4
r4 :: Int
r4 = a_plus_b (\ x -> case x of 
                       "a" -> 1
                       "b" -> 3
              ) 

-- оператор "a := 8"
a_assign_8 :: Stmt
a_assign_8 = "a" <:= lit 8

-- дает состояние, в котором "a" равно "8"
r8 :: Int
r8 = a_assign_8 (\ _ -> 3) "a"

-- сумма чисел до данного включительно
sum :: Int -> Stmt
sum n =
  "sum" <:= lit 0 !>
  "i"   <:= lit n !>
  while (var "i" >! lit 0)
        ("sum" <:= var "sum" +! var "i" !>
         "i"   <:= var "i"   -! lit 1)

-- применяем к параметру 4 и пустому состоянию и получаем состояние, 
-- в котором значение переменной "sum" равно 10
r10 :: Int
r10 = Imp.sum 4 undefined "sum"

-- Написать вычисление факториала. Результат -- оператор и имя переменной,
-- в которой сохраняется ответ
fact :: Int -> (Stmt, String)
fact n = (f n, "p") where 
  f n = 
        "p" <:= lit 1 !>
		"i" <:= lit n !>
		while (var "i" >! lit 0)
		  ("p" <:= var "p" *! var "i" !>
		   "i" <:= var "i" -! lit 1)
		
{-fact = READ "n" :>>:
       "f" ::=: C 1 :>>:
       WHILE (X "n" :>: C 0) 
         ("f" ::=: X "f" :*: X "n" :>>:
          "n" ::=: X "n" :-: C 1 
         ) :>>:
       WRITE (X "f")-}