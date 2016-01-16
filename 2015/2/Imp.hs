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
--var :: String -> ((String -> Int) -> Int)
var name = (\f -> f name)

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
-- 0 - false, otherwise - true.
(&&&), (|||), (<!), (>!), (===), (=/=), (+!), (-!), (*!) :: Expr -> Expr -> Expr
-- :: (State -> Int) -> (State -> Int) -> (State -> Int)
--(&&&) x y = (\f -> if (x f /= 0 && y f /= 0) then 1 else 0)
(&&&) x y = x *! y
(|||) x y = x +! y
(<!)  x y = (\f -> if (x f < y f) then 1 else 0)
(>!)  x y = (\f -> if (x f > y f) then 1 else 0)
(===) x y = (\f -> if (x f == y f) then 1 else 0)
(=/=) x y = (\f -> if (x f /= y f) then 1 else 0)
(+!)  x y = (\f -> x f + y f)
(-!)  x y = (\f -> x f - y f)
(*!)  x y = (\f -> x f * y f)

-- Операторы
-- присваивание
infix 2 <:=

(<:=) :: String -> Expr -> Stmt
(<:=) name val = (\f -> (\x -> if (x == name) then val f else f x))

-- последовательое исполнение
infixr 1 !>

(!>) :: Stmt -> Stmt -> Stmt
(!>) s1 s2 = s2 . s1

-- ветвление (if-then-else)
branch :: Expr -> Stmt -> Stmt -> Stmt
branch cond s1 s2 = (\f -> if (cond f /= 0) then s1 f else s2 f)
                  
-- цикл с предусловием                  
while :: Expr -> Stmt -> Stmt
while cond s = while' cond s s where
    while' cond s s' = (\f -> if (cond (s' f) /= 0) then while' cond s (s' . s) f else s' f)
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

test' :: Stmt
test' = 
    "x" <:= lit 1 !>
    while (var "x" >! lit 0)
          ("x" <:= var "x" -! lit 1)
          
          
test2 :: Int -> Stmt
test2 x = 
    "x" <:= lit x !>
    branch (var "x" >! lit 5) 
        ("x" <:= var "x" -! lit 1)
        ("x" <:= var "x" +! lit 1)
        
test3 :: Stmt
test3 = 
    "x" <:= lit 10 !>
    ("x" <:= var "x" -! lit 1) . ("x" <:= var "x" -! lit 1)
    
test4 :: Int -> Stmt
test4 x = 
    "x" <:= lit x !>
    while (var "x" >! lit 0) 
        ("x" <:= var "x" -! lit 1)
-- Написать вычисление факториала. Результат -- оператор и имя переменной,
-- в которой сохраняется ответ
fact :: Int -> (Stmt, String)
fact x = (
    "x" <:= lit x !>
    "fact" <:= lit 1 !>
    while (var "x" >! lit 1) 
        ("fact" <:= var "fact" *! var "x" !>
         "x" <:= var "x" -! lit 1)
    ,
    "fact"
    )