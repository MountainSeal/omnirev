module ExhaustiveSpec where

import Omnirev.AbsOmnirev
import Omnirev.CheckOmnirev ( (~>) )
import Omnirev.Exhaustive
import Omnirev.ErrM as Err
import Test.Hspec
import Test.Hspec.QuickCheck ( prop )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Prelude hiding ((++), (*), id, not, (&&), (**), (*>), (||))
import Control.Monad ( liftM, liftM2 )
import Utils


spec :: Spec
spec =do
  describe "decompose" $ do
    prop "return Nothing if Type is variable" $ do
      \x -> decompose (tyvar (x::String)) `shouldBe` Nothing
    it "unit" $
      decompose unit `shouldBe` Just (SpKonst "unit" [])
    prop "sum" $
      \t1 t2 -> decompose (t1 `TySum` t2) `shouldBe` Just (SpSum (SpKonst "inl" [SpType (t1 :: Type)]) (SpKonst "inr" [SpType (t2 :: Type)]))
    prop "tensor" $
      \t1 t2 -> decompose (t1 `TyTensor` t2) `shouldBe` Just (SpKonst "tensor" [SpType (t1 :: Type), SpType (t2 :: Type)])
    prop "arrow" $
      \t1 t2 -> decompose (t1 `TyFunc` t2) `shouldBe` Just (SpKonst "arrow" [SpType (t1 :: Type), SpType (t2 :: Type)])
    prop "fold" $
      \x t -> decompose (TyRec (x :: Ident) (t :: Type)) `shouldBe` Just (SpKonst "fold" [SpType $(x ~> TyRec x t) t])

  describe "project" $ do
    it "return type space for variable" $
      project (tmvar "x") natlist `shouldBe` SpType natlist
    it "return " $
      project notandid ((qubit ** qubit) *> (qubit ** qubit)) `shouldNotBe` SpEmpty
    it "return " $
      project add ((nat ** nat) *> (nat ** nat)) `shouldNotBe` SpEmpty

  describe "ioput" $ do
    it "return " $
      ioput notandid ((qubit ** qubit) *> (qubit ** qubit)) `shouldNotBe` Just (SpEmpty, SpEmpty)
    it "" $
      ioput add ((nat ** nat) *> (nat ** nat)) `shouldNotBe` Just (SpEmpty, SpEmpty)

  describe "exhaust" $ do
    it "not function is exhaustive" $
      exhaust not circuit `shouldBe` True
    it "add function is exhaustive" $
      exhaust add ((nat ** nat) *> (nat ** nat)) `shouldBe` True