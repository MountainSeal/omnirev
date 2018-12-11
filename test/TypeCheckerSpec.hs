module TypeCheckerSpec where

import Omnirev.AbsOmnirev
import Omnirev.TypeChecker as TC
import Omnirev.ErrM as Err
import Test.Hspec

spec :: Spec
spec = do
  describe "typeChecker" $ do
    it "check" $
      check (Prog [DType (Ident "qubit") (TSum TUnit TUnit)]) `shouldBe` "Success!"
  describe "typeChecker" $ do
    it "check" $
      check (Prog [DType (Ident "qubit") (TSum TUnit TUnit),DType (Ident "byte") (TTensor (TTensor (TTensor (TVar (Ident "qubit")) (TVar (Ident "qubit"))) (TVar (Ident "qubit"))) (TVar (Ident "qubit")))]) `shouldBe` "Success!"