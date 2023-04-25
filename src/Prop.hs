module Prop where

import Data.List (nub)

data Prop
  = Implies Prop Prop
  | And Prop Prop
  | Or Prop Prop
  | Not Prop
  | Name Name
  | Con Bool deriving (Eq, Show)

type Name = String

type Env = Name -> Bool

eval :: Prop -> Env -> Bool
eval (Implies p q) env = eval p env ==> eval q env
eval (And p q) env = eval p env && eval q env
eval (Or p q) env = eval p env || eval q env
eval (Not p) env = not (eval p env)
eval (Name n) env = env n
eval (Con t) _ = t

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True ==> p = p

isTautology :: Prop -> Bool
isTautology p = and (eval p <$> envs (freeNames p))

envs :: [Name] -> [Env]
envs [] = [error "envs: never used"]
envs (n : ns) = [\n' -> if n == n' then b else e n' | b <- [False, True], e <- envs ns]

freeNames :: Prop -> [Name]
freeNames = nub . go []
  where
    go acc (Name n) = n : acc
    go acc (Not p) = go acc p
    go acc (Implies p q) = go acc p ++ go acc q
    go acc (And p q) = go acc p ++ go acc q
    go acc (Or p q) = go acc p ++ go acc q
    go acc (Con _) = acc

