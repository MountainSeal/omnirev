module EvalOmnirevSpec where

import Omnirev.AbsOmnirev
import Omnirev.EvalOmnirev
import Omnirev.ErrM as Err
import Test.Hspec
import Data.Map as Map

spec :: Spec
spec = do
  describe "check" $
    it "type" $
      check (Prog [DType (Ident "alias") TyUnit
                  ,DType (Ident "qubit") (TySum TyUnit TyUnit)
                  ,DType (Ident "tens") (TyTensor (TySum TyUnit TyUnit) (TySum TyUnit TyUnit))
                  ,DType (Ident "var") (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))
                  ,DType (Ident "circuit") (TyFunc (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))
                  ,DType (Ident "nat") (TyRec (Ident "x") (TySum TyUnit (TyVar (Ident "x"))))
                  ,DType (Ident "natlist") (TyRec (Ident "x") (TySum TyUnit (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "x")))))
                  ])
        `shouldBe` "Success!"
  describe "example" $
    it "hoge" $
      check (Prog [DType (Ident "qubit") (TySum TyUnit TyUnit)
                  ,DTerm (Ident "not") (TyFunc (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (ExTerm (TmLin (TmArrow (TmLeft TmUnit) (TmRight TmUnit)) (TmArrow (TmRight TmUnit) (TmLeft TmUnit))))
                  ,DTerm (Ident "cnot") (TyFunc (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))) (ExTerm (TmLin (TmLin (TmLin (TmArrow (TmTensor (TmLeft TmUnit) (TmLeft TmUnit)) (TmTensor (TmLeft TmUnit) (TmLeft TmUnit))) (TmArrow (TmTensor (TmLeft TmUnit) (TmRight TmUnit)) (TmTensor (TmLeft TmUnit) (TmRight TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmLeft TmUnit)) (TmTensor (TmRight TmUnit) (TmRight TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmRight TmUnit)) (TmTensor (TmRight TmUnit) (TmLeft TmUnit)))))
                  ,DType (Ident "nat") (TyRec (Ident "X") (TySum TyUnit (TyVar (Ident "X"))))
                  ,DTerm (Ident "add") (TyFunc (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat")))) (ExTrace (TmLin (TmLin (TmArrow (TmTensor (TmVar (Ident "x")) (TmVar (Ident "y"))) (TmLabel (Ident "iter") (TmTensor (TmVar (Ident "y")) (TmTensor (TmFold (TmLeft TmUnit)) (TmVar (Ident "x")))))) (TmArrow (TmLabel (Ident "iter") (TmTensor (TmVar (Ident "a")) (TmTensor (TmVar (Ident "b")) (TmFold (TmLeft TmUnit))))) (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))))) (TmArrow (TmLabel (Ident "iter") (TmTensor (TmVar (Ident "a")) (TmTensor (TmVar (Ident "b")) (TmFold (TmRight (TmVar (Ident "n"))))))) (TmLabel (Ident "iter") (TmTensor (TmFold (TmRight (TmVar (Ident "a")))) (TmTensor (TmFold (TmRight (TmVar (Ident "b")))) (TmVar (Ident "n"))))))) (Ident "iter") (TyTensor (TyVar (Ident "nat")) (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat")))))
                  ])
        `shouldBe` "Success!"