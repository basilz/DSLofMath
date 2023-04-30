{-# LANGUAGE NoImplicitPrelude #-}

module Ex3_7 where
import GHC.Base (undefined, (.))

{- 
If z = g(y) and y = h(x) are two functions with continuous derivatives, then in the relevant range z = g(h(x)) is 
a function of x and has derivative: z' = g'(y) * h'(x)
-}

(deriv) = undefined

data X
data Y
data Z

x :: X
x = undefined

y :: Y
y = undefined

z :: X -> Z
z = g . h

g :: Y -> Z
g = undefined

h :: X -> Y
h = undefined

h' :: (X -> Y) -> X -> Y
h' h = deriv h

g' :: (Y -> Z) -> Y -> Z
g' g = deriv g

z' :: (Y -> Z) -> (X -> Y) -> X -> Z
z' gg hh = g' gg * h' hh

(*) :: (Y -> Z) -> (X -> Y) -> X -> Z
(*) = undefined