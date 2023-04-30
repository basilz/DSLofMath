{-# LANGUAGE TypeOperators #-}
module TypeCheck where

type Truth = ()

data False = False

type And a b = (a, b)

type Or a b = Either a b

type Implies p q = p -> q

type Not p = p `Implies` False

truthIntro :: Truth
truthIntro = ()

andIntro :: p -> q -> And p q
andIntro p q = (p, q)

orIntroL :: p -> Or p q
orIntroL = Left

orIntroR :: q -> Or p q
orIntroR = Right

notIntro :: And (Implies p q) (Implies p (Not q)) -> Not p
notIntro (f, g) x = g x (f x)

implyIntro :: (p -> q) -> p `Implies` q
implyIntro f = f

-- falseElim :: False -> p
-- falseElim x = case x of

andElimL :: And p q -> p
andElimL = fst

andElimR :: And p q -> q
andElimR = snd

orElim :: p `Implies` r -> q `Implies` r -> p `Or` q -> r
orElim f g pOrq = case pOrq of
  Left p -> f p
  Right q -> g q

-- notElim :: Not(Not p) -> p
-- notElim = undefined

implyElim :: (p `Implies` q) -> p -> q
implyElim f = f

excludedMiddle :: Not (Not (p `Or` Not p)) -- (((Either p (p -> False)) -> False) -> False)
excludedMiddle k = k (Right (k . Left))

test1 :: (a -> (b, c)) -> (a -> b, a -> c)
test1 f = (\x -> fst $ f x, snd . f)

test1' :: Implies (Implies a (And b c)) (And (Implies a b) (Implies a c))
test1' =
  implyIntro
    ( \f ->
        andIntro
          (implyIntro (\x -> andElimL (implyElim f x)))
          (implyIntro (\x -> andElimR (implyElim f x)))
    )

test2 :: (a -> b, a -> c) -> (a -> (b, c))
test2 (f, g) = \x -> (f x, g x)

test2' :: Implies (And (Implies a b) (Implies a c)) (Implies a (And b c))
test2' =
  implyIntro
    ( \fg ->
        implyIntro
          ( \x ->
              andIntro (implyElim (andElimL fg) x) (implyElim (andElimR fg) x)
          )
    )


(x, deriv, ff, a, b, int) = undefined

data X
data Y
data Z

f :: X -> Y
f = undefined

x :: X

y :: X -> Y
y = f

y' :: X -> Y
y' = deriv f

deriv :: (X -> Y) -> (X -> Y)

ff :: (X -> Y) -> (X -> Y) -> X -> Z

a::X
b::X

integral :: Z
integral = int a b expr 
  where 
    expr x = ff y y' x

ff2 :: Y -> Y -> X -> Z
ff2 = undefined




