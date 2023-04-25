import Prop
import Test.Hspec

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
