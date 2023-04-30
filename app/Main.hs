module Main (main) where

import PropositionalLogic
import Proof


conjProp :: Prop
conjProp = Implies (And (Name "a") (Name "b")) (And (Name "b") (Name "a"))

conjPropProof :: Proof
conjPropProof = ImplyIntro step
    where
        step evAB = AndIntro (AndElimL (Name "b") evAB) (AndElimR (Name "a") evAB)
    

main :: IO ()
main = print $ checkProof conjPropProof conjProp
