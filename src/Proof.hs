module Proof where

import PropositionalLogic ( Prop(Implies, And, Or, Not, Con) )

data Proof
  = TruthIntro
  | AndIntro Proof Proof
  | OrIntroL Proof
  | OrIntroR Proof
  | NotIntro Prop Proof Proof
  | ImplyIntro (Proof -> Proof)
  | FalseElim Proof
  | AndElimL Prop Proof
  | AndElimR Prop Proof
  | OrElim Prop Prop Proof Proof Proof
  | NotElim Proof
  | ImplyElim Prop Proof Proof
  | Assume Prop

checkProof :: Proof -> Prop -> Bool
checkProof TruthIntro (Con True) = True
checkProof (AndIntro t u) (And p q) = checkProof t p && checkProof u q
checkProof (OrIntroL t) (Or p _) = checkProof t p
checkProof (OrIntroR t) (Or _ p) = checkProof t p
checkProof (NotIntro q t u) (Not p) = checkProof t (p `Implies` q) && checkProof u (p `Implies` Not q)
checkProof (AndElimL q t) p = checkProof t (p `And` q)
checkProof (AndElimR p t) q = checkProof t (p `And` q)
checkProof (OrElim p q t u v) r = checkProof t (p `Implies` r) && checkProof u (q `Implies` r) && checkProof v (p `Or` q)
checkProof (NotElim t) p = checkProof t (Not (Not p))
checkProof (FalseElim t) _ = checkProof t (Con False)
checkProof (ImplyIntro f) (p `Implies` q) = checkProof (f (Assume p)) q
checkProof (ImplyElim p t u) q = checkProof t (p `Implies` q) && checkProof u p
checkProof (Assume p') p = p == p'
checkProof _ _ = False -- Any other option is an incorrect proof