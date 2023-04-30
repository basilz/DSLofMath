module Ex3_8 where

{-
In these cases one tries to find not the values of \(x\) which
make a given function \(y = f(x)\) a minimum, but the values of a given
function \(f(x)\) which make a given quantity a minimum.  Typically,
that quantity is usually measured by an integral whose integrand is
some expression \(F\) involving both \(x\), values of the function \(y =
f(x)\) at interest and the values of its derivatives -- say an
integral

   \[∫_a^b F(y, y', x)dx,\quad y = f(x).\]

Give the types of the variables involved (|x|, |y|, |y'|, |f|, |F|,
|a|, |b|) and the type of the four-argument integration operator:

   \[∫_.^. \cdot d\cdot\]

-}

data X

data Y

data Z

x :: X
x = undefined

f :: X -> Y
f = undefined

y :: X -> Y
y = f

deriv :: (X -> Y) -> (X -> Y)
deriv _f = undefined

y' :: X -> Y
y' = deriv f

ff :: (X -> Y) -> (X -> Y) -> X -> Z
ff _y _y' _x = undefined

int :: X -> X -> (X -> Z) -> Z
int = undefined

a, b :: X
(a, b) = undefined

integral :: Z
integral = int a b expr
  where
    expr = ff y y'
