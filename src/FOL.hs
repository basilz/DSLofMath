module FOL where

type VarT = String

data RatT = RV VarT | FromI Integer | RPlus RatT RatT | RDiv RatT RatT deriving (Show)

type RatSem = Rational

type PSym = String

data FOL
  = Implies FOL FOL
  | And FOL FOL
  | Or FOL FOL
  | Not FOL
  | ForAll VarT FOL
  | Exists VarT FOL
  | PName PSym [RatT]
  | Equal RatT RatT
  deriving (Show)

evalRat :: RatT -> (VarT -> RatSem) -> RatSem
evalRat (RV v) env = env v
evalRat (FromI x) _ = fromInteger x
evalRat (RPlus x y) env = evalRat x env + evalRat y env
evalRat (RDiv x y) env = evalRat x env / evalRat y env

eval0 :: PSym -> [RatSem] -> Bool
eval0 "Equal" [x, y] = x == y
eval0 "LessThan" [x, y] = x < y
eval0 "Positive" [x] = x > 0

eval :: FOL -> (VarT -> RatSem) -> Bool
eval formula env = ev formula
  where
    ev (PName n args) = eval0 n (map (flip evalRat env) args)
    ev (Equal a b) = evalRat a env == evalRat b env
    ev (And a b) = ev a && ev b
    ev (Or a b) = ev a || ev b

type AllIntro f a = f -> a
data AllElim f a = AllElim f a 
data Forall x p = Forall x p


proofChecker (AllIntro f a) (Forall x p) = proofChecker (f a') (subst x a' p)
  where
    a' = freshFor [a, Forall x p]
proofChecker (AllElim f t) p =
  checkUnify (f x') p
    && proofChecker t (Forall x' (f x'))
  where
    x' = freshFor [f x, p]
