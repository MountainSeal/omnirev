module TypeCheckerSpec where

import Omnirev.AbsOmnirev
import Omnirev.TypeChecker as TC
import Omnirev.ErrM as Err
import Test.Hspec
import Data.Map as Map

spec :: Spec
spec = do
  describe "check" $ do
    it "type" $
      check (Prog [DType (Ident "alias") TUnit
                  ,DType (Ident "qubit") (TSum TUnit TUnit)
                  ,DType (Ident "tens") (TTensor TUnit TUnit)
                  ,DType (Ident "tstr") (TStar TUnit)
                  ,DType (Ident "tvar") (TVar (Ident "qubit"))
                  ])
        `shouldBe` "Success!"
    it "function" $
      check (Prog [DType (Ident "qubit") (TSum TUnit TUnit)
                  ,DType (Ident "octet") (TTensor (TTensor (TTensor (TTensor (TTensor (TTensor (TTensor (TVar (Ident "qubit")) (TVar (Ident "qubit"))) (TVar (Ident "qubit"))) (TVar (Ident "qubit"))) (TVar (Ident "qubit"))) (TVar (Ident "qubit"))) (TVar (Ident "qubit"))) (TVar (Ident "qubit")))
                  ,DFunc (Ident "fId") TUnit TUnit FId
                  ,DFunc (Ident "fTensor") (TTensor TUnit TUnit) (TTensor TUnit TUnit) (FTensor FId FId)
                  ,DFunc (Ident "fTensUnit") (TTensor TUnit (TVar (Ident "qubit"))) (TVar (Ident "qubit")) FTensUnit
                  ,DFunc (Ident "ftensassoc")
                    (TTensor (TVar (Ident "qubit")) (TTensor (TVar (Ident "qubit")) (TVar (Ident "qubit"))))
                    (TTensor (TTensor (TVar (Ident "qubit")) (TVar (Ident "qubit"))) (TVar (Ident "qubit")))
                   FTensAssoc
                  ,DFunc (Ident "ftenssym")
                    (TTensor TUnit (TVar (Ident "qubit")))
                    (TTensor (TVar (Ident "qubit")) TUnit)
                   FTensSym
                  ,DFunc (Ident "fsum") (TSum TUnit TUnit) (TSum TUnit TUnit) (FSum FId FId)
                  ,DFunc (Ident "fsumassoc")
                    (TSum (TVar (Ident "qubit")) (TSum (TVar (Ident "qubit")) (TVar (Ident "qubit"))))
                    (TSum (TSum (TVar (Ident "qubit")) (TVar (Ident "qubit"))) (TVar (Ident "qubit")))
                   FSumAssoc
                  ,DFunc (Ident "fsumsym")
                    (TSum (TVar (Ident "qubit")) TUnit)
                    (TSum TUnit (TVar (Ident "qubit")))
                   FSumSym
                  ,DFunc (Ident "fdistrib")
                    (TTensor (TSum TUnit (TVar (Ident "qubit"))) (TVar (Ident "octet")))
                    (TSum (TTensor TUnit (TVar (Ident "octet"))) (TTensor (TVar (Ident "qubit")) (TVar (Ident "octet"))))
                   FDistrib
                  ,DFunc (Ident "feval")
                    (TTensor (TVar (Ident "qubit")) (TStar (TVar (Ident "qubit"))))
                    TUnit
                   (FEval (TVar (Ident "qubit")))
                  ,DFunc (Ident "tvar") (TSum TUnit TUnit) (TSum TUnit TUnit) (FVar (Ident "fsum"))
                  ,DFunc (Ident "fcomp")
                    (TTensor (TSum TUnit (TVar (Ident "qubit"))) (TVar (Ident "octet")))
                    (TSum (TTensor TUnit (TVar (Ident "octet"))) (TTensor (TVar (Ident "octet")) (TVar (Ident "qubit"))))
                   (FComp FDistrib (FSum FId FTensSym))
                  ,DFunc (Ident "fdagger")
                    (TSum (TTensor TUnit (TVar (Ident "octet"))) (TTensor (TVar (Ident "octet")) (TVar (Ident "qubit"))))
                    (TTensor (TSum TUnit (TVar (Ident "qubit"))) (TVar (Ident "octet")))
                   (FDagger (FComp FDistrib (FSum FId FTensSym)))
                  ,DFunc (Ident "fshft") (TVar (Ident "qubit")) (TVar (Ident "qubit")) (FShift 45)
                  ])
      `shouldBe` "Success!"
    it "type not found error" $
      check (Prog [DType (Ident "qubit") (TSum TUnit TUnit)
                  ,DFunc (Ident "fdistrib")
                    (TTensor (TSum TUnit (TVar (Ident "qubit"))) (TVar (Ident "octet")))
                    (TSum (TTensor TUnit (TVar (Ident "octet"))) (TTensor (TVar (Ident "qubit")) (TVar (Ident "octet")))) FDistrib
                  ])
        `shouldNotBe` "Success!"
    it "definition conflict error" $
      check (Prog [DType (Ident "qubit") (TSum TUnit TUnit),DType (Ident "qubit") (TSum (TSum TUnit TUnit) TUnit)])
      `shouldNotBe` "Success!"
    it "type variable" $
      check (Prog [DType (Ident "qubit") (TSum TUnit TUnit)
                  ,DFunc (Ident "test") (TVar (Ident "qubit")) (TSum TUnit TUnit) FId
                  ])
      `shouldBe` "Success!"
  describe "purify" $
    it "type" $ do
      purify (TVar (Ident "qubit")) (Map.fromList [("qubit", TSum TUnit TUnit)])
        `shouldBe` Just (TSum TUnit TUnit)
      purify (TTensor TUnit (TVar (Ident "qubit"))) (Map.fromList [("qubit", TSum TUnit TUnit)])
        `shouldBe` Just (TTensor TUnit (TSum TUnit TUnit))
