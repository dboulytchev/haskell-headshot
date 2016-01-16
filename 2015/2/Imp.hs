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
lit x = \state -> x 

-- переменная
var :: String -> Expr
var str = \f -> f str

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

(&&&) exp1 exp2 = \st -> if ((exp1 st) /= 0) && ((exp2 st) /= 0) then 1 else 0
(|||) exp1 exp2 = \st -> if ((exp1 st) /= 0) || ((exp2 st) /= 0) then 1 else 0
(<!)  exp1 exp2 = \st -> if ((exp1 st) < (exp2 st)) then 1 else 0
(>!)  exp1 exp2 = \st -> if ((exp1 st) > (exp2 st)) then 1 else 0
(===) exp1 exp2 = \st -> if ((exp1 st) == (exp2 st)) then 1 else 0
(=/=) exp1 exp2 = \st -> if ((exp1 st) /= (exp2 st)) then 1 else 0
(+!)  exp1 exp2 = \st -> (exp1 st) + (exp2 st)
(-!)  exp1 exp2 = \st -> (exp1 st) - (exp2 st)
(*!)  exp1 exp2 = \st -> (exp1 st) * (exp2 st)

-- Операторы
-- присваивание
infix 2 <:=

(<:=) :: String -> Expr -> Stmt
(<:=)  str expr = \state x -> if (x == str) then expr state 
                                            else state x

-- последовательное исполнение
infixr 1 !>

(!>) :: Stmt -> Stmt -> Stmt
(!>) stmt1 stmt2 = \state -> stmt2 ( stmt1 state)

-- ветвление (if-then-else)
branch :: Expr -> Stmt -> Stmt -> Stmt
branch expr stmt1 stmt2 = \state -> if ((expr state) /= 0) then stmt1 state
                                                           else stmt2 state
                  
-- цикл с предусловием                  
while :: Expr -> Stmt -> Stmt
while expr stmt1 = \state -> if ((expr state) == 0) then state
                                                    else while expr stmt1 (stmt1  state)

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
fact' n =
   "i"   <:= lit n !>
   "prod" <:= lit 1 !>
   while (var "i" >! lit 0)
        ("prod" <:= var "prod" *! var "i" !>
         "i"   <:= var "i"   -! lit 1)
fact :: Int -> (Stmt, String)
fact n = (Imp.fact' n, "prod")

