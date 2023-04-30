import PropositionalLogic
import Test.Hspec
import Proof (checkProof, Proof (ImplyIntro, AndElimR, OrElim, OrIntroL, OrIntroR, Assume))


proof1 :: Proof
proof1 = ImplyIntro (AndElimR (Name "p"))

proof2 :: Proof
proof2 = ImplyIntro (OrElim (Name "p") (Name "q") (ImplyIntro OrIntroR) (ImplyIntro OrIntroL))

proof3 :: Proof
proof3 = undefined

main :: IO ()
main = hspec $ do
  describe "Tautologies" $ do
    it "'a and !a' is not a tautology" $ do
      not $ isTautology (And (Name "a") (Not (Name "a")))
    it "'a or !a' is a tautology" $ do
      isTautology (Or (Name "a") (Not (Name "a")))
    it "'a => b' is not a tautology" $ do
      not $ isTautology (Name "a" `Implies` Name "b")
    it "'a and b => b and a' is a tautology" $ do
      isTautology ((Name "a" `And` Name "b") `Implies` (Name "b" `And` Name "a"))
  describe "Theorem Proving" $ do
    it "p AND q -> q" $ do
      checkProof proof1 (Implies (And (Name "p") (Name "q")) (Name "q"))
    it "p OR q -> q Or p" $ do
      checkProof proof2 (Implies (Or (Name "p") (Name "q")) (Or (Name "q") (Name "p")))
    it "(p -> q) -> (!q -> !p)" $ do
      checkProof proof3 (Implies (Implies (Name "p") (Name "q")) (Implies (Not (Name "q")) (Not (Name "p"))))