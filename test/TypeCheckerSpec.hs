module TypeCheckerSpec where

import Omnirev.AbsOmnirev
import Omnirev.TypeChecker as TC
import Omnirev.ErrM as Err
import Test.Hspec

spec :: Spec
spec = 
  describe "typeChecker" $ do
    it "check type" $ do
      check (Prog [DType (Ident "qubit") (TSum TUnit TUnit)]) `shouldBe` "Success!"
      check (Prog [DType (Ident "alias") TUnit,DType (Ident "qubit") (TSum TUnit TUnit),DType (Ident "tens") (TTensor TUnit TUnit),DType (Ident "str") (TStar TUnit),DType (Ident "tvar") (TVar (Ident "qubit"))]) `shouldBe` "Success!"
    it "check function" $ 
      check (Prog [DType (Ident "qubit") (TSum TUnit TUnit),DFunc (Ident "test_id") TUnit TUnit FId,DFunc (Ident "test2") (TTensor TUnit TUnit) (TTensor TUnit TUnit) (FTensor FId FId),DFunc (Ident "test3") (TSum TUnit TUnit) (TSum TUnit TUnit) (FSum FId FId),DFunc (Ident "test4") (TVar (Ident "qubit")) (TVar (Ident "qubit")) FId]) `shouldBe` "Success!"