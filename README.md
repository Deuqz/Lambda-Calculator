# Lambda-Calculator

Целью работы является построения лямбда-калькулятора, то есть библиотеки, 
которая позволяет β-редуцировать лямбда-термы и помогать решать вопросы их αβ-эквивалентности.

Следующий тип данных будет использоваться для описания термов чистого нетипизированного лямбда-иcчиcления:

```
type Symb = String 

infixl 2 :@

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)
```

Например, комбинатор `ω = λx.xx` в этом представлении будет иметь вид `Lam "x" (Var "x" :@ Var "x")`.

Для тестирования можно использовать библиотеку комбинаторов из файла Combinators.hs.

Функция `subst :: Symb -> Expr -> Expr -> Expr` реализуйте алгоритм подстановки терма 
n вместо всех свободных вхождений переменной v в терме m (в стандартной нотации m\[v:=n\]). 
Вспомогательная функция `freeVars :: Expr -> [Symb]` возвращает список свободных переменных терма. 
Пример работы:

```
GHCi> subst "y"  (Var "x")  (Lam "x" $ (Var "x") :@ (Var "y"))
Lam "x'" (Var "x'" :@ Var "x")
```

Оператор `alphaEq :: Expr -> Expr -> Bool` реализует алгоритм проверки α-эквивалентности двух термов:

```
GHCi> (Lam "x" $ Lam "y" $ Var "x") `alphaEq` (Lam "y" $ Lam "x" $ Var "y")
True
```

Функция `reduceOnce :: Expr -> Maybe Expr` реализует алгоритм одношаговой β-редукции, 
сокращающий самый левый внешний редекс в терме, если это возможно.

```
GHCi> reduceOnce $ Lam "x" $ Lam "y" $ Var "x"
Nothing
GHCi> reduceOnce $ Lam "x" $ Lam "y" $ (Lam "z" $ Var "z") :@ Var "x"
Just (Lam "x" (Lam "y" (Var "x")))
GHCi> let omega = Lam "x" $ Var "x" :@ Var "x" in reduceOnce $ omega :@ omega
Just (Lam "x" (Var "x" :@ Var "x") :@ Lam "x" (Var "x" :@ Var "x"))
```

Функция `nf :: Expr -> Expr` реализует алгоритм многошаговой β-редукции к нормальной форме, 
использующий нормальную стратегию:

```
GHCi> nf (fac :@ three) `alphaEq` six
True
```

Оператор `betaEq :: Expr -> Expr -> Bool` реализует алгоритм проверки β-эквивалентности двух термов:

```
GHCi> fac :@ three `betaEq` six
True
```

Тип данных Expr инстнцируется представителем классов типов Show и Read. Для этого применяются парсер комбинаторы из 
библиотеки Parsec.

Представитель Show реализован так, что строковое представление является валидным лямбда-термом в синтаксисе Haskell:

```
GHCi> show $ Lam "x" (Var "x" :@ Var "y")
"\\x -> x y"
GHCi> cY = let {x = Var "x"; f = Var "f"; fxx = Lam "x" $ f :@ (x :@ x)} in Lam "f" $ fxx :@ fxx
GHCi> show cY
"\\f -> (\\x -> f (x x)) (\\x -> f (x x))"
GHCi> cY
\f -> (\x -> f (x x)) (\x -> f (x x))
```

Представитель Read реализован так, что валидный в синтаксисе Haskell чистый лямбда-терм считывается 
в соответствующее выражение типа Expr:

```
GHCi> (read "\\x1 x2 -> x1 x2 x2" :: Expr) == Lam "x1" (Lam "x2" (Var "x1" :@ Var "x2" :@ Var "x2"))
True
GHCi> read (show cY) == cY
True
```