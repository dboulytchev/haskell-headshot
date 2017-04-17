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
lit x = (\_ -> x)

-- переменная
var :: String -> Expr
var a = (\f -> f a) 

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

(&&&) a b = (\f -> if (a f == 0 || b f == 0) then 0 else 1)
(|||) a b = (\f -> if (a f == 0 && b f == 0) then 0 else 1)
(<!)  a b = (\f -> if (a f < b f) then 1 else 0)
(>!)  a b = (\f -> if (a f > b f) then 1 else 0)
(===) a b = (\f -> if (a f == b f) then 1 else 0)
(=/=) a b = (\f -> if (a f /= b f) then 1 else 0)
(+!)  a b = (\f -> a f + b f)
(-!)  a b = (\f -> a f - b f)
(*!)  a b = (\f -> a f * b f)

-- Операторы
-- присваивание
infix 2 <:=

(<:=) :: String -> Expr -> Stmt
(<:=) a b = (\f x -> if (x == a) then b f else f x)

-- последовательое исполнение
infixr 1 !>

(!>) :: Stmt -> Stmt -> Stmt
(!>) a b = (\f -> b ( a f))

-- ветвление (if-then-else)
branch :: Expr -> Stmt -> Stmt -> Stmt
branch a b c = (\f -> if ((a f) /= 0) then b f
                                    else c f)
                  
-- цикл с предусловием                  
while :: Expr -> Stmt -> Stmt
while a b = (\f -> if ((a f) == 0) then f
                                   else while a b (b f))

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
fact n = (Imp.fact' n, "res")
fact' n =
    "res" <:= lit 1 !>
	"i"    <:= lit n !>
	while (lit 0 <! var "i")
	       ("res" <:= var "res" *! var "i" !>
		   "i"    <:= var "i"    -! lit 1)
