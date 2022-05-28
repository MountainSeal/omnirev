module CheckOmnirevSpec where

import Omnirev.AbsOmnirev
import Omnirev.CheckOmnirev
import Omnirev.ErrM as Err
import Test.Hspec
import Data.Map as Map
import Utils

spec :: Spec
spec = do
  describe "check" $ do
    it "type" $
      check (Prog [DType (Ident "alias") TyUnit
                  ,DType (Ident "qubit") (TySum TyUnit TyUnit)
                  ,DType (Ident "tens") (TyTensor (TySum TyUnit TyUnit) (TySum TyUnit TyUnit))
                  ,DType (Ident "var") (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))
                  ,DType (Ident "circuit") (TyFunc (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))
                  ,DType (Ident "nat") (TyRec (Ident "x") (TySum TyUnit (TyVar (Ident "x"))))
                  ,DType (Ident "natlist") (TyRec (Ident "y") (TySum TyUnit (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "y")))))
                  ])
        `shouldNotBe` Bad ""
    it "term" $
      check (Prog [DType (Ident "qubit") (TySum TyUnit TyUnit)
                  ,DTerm (Ident "not") (TyFunc (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TmLin (TmArrow (TmLeft TmUnit) (TmRight TmUnit)) (TmArrow (TmRight TmUnit) (TmLeft TmUnit)))
                  ,DTerm (Ident "swap") (TyFunc (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))) (TmLin (TmLin (TmLin (TmArrow (TmTensor (TmLeft TmUnit) (TmLeft TmUnit)) (TmTensor (TmLeft TmUnit) (TmLeft TmUnit))) (TmArrow (TmTensor (TmLeft TmUnit) (TmRight TmUnit)) (TmTensor (TmRight TmUnit) (TmLeft TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmLeft TmUnit)) (TmTensor (TmLeft TmUnit) (TmRight TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmRight TmUnit)) (TmTensor (TmRight TmUnit) (TmRight TmUnit))))
                  ,DTerm (Ident "cnot") (TyFunc (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))) (TmLin (TmLin (TmLin (TmArrow (TmTensor (TmLeft TmUnit) (TmLeft TmUnit)) (TmTensor (TmLeft TmUnit) (TmLeft TmUnit))) (TmArrow (TmTensor (TmLeft TmUnit) (TmRight TmUnit)) (TmTensor (TmLeft TmUnit) (TmRight TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmLeft TmUnit)) (TmTensor (TmRight TmUnit) (TmRight TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmRight TmUnit)) (TmTensor (TmRight TmUnit) (TmLeft TmUnit))))
                  ,DTerm (Ident "notandid") (TyFunc (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))) (TmComp (TmComp (TmVar (Ident "not")) TmId) (TmFlip (TmVar (Ident "not"))))
                  ,DType (Ident "nat") (TyRec (Ident "X") (TySum TyUnit (TyVar (Ident "X"))))
                  ,DTerm (Ident "add") (TyFunc (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat")))) (TmTrace (TyVar (Ident "nat")) (TmLin (TmLin (TmArrow (TmRight (TmTensor (TmVar (Ident "x")) (TmVar (Ident "y")))) (TmLeft (TmTensor (TmTensor (TmVar (Ident "y")) (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit))) (TmVar (Ident "x"))))) (TmArrow (TmLeft (TmTensor (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))) (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "n")))))) (TmLeft (TmTensor (TmTensor (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "a")))) (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "b"))))) (TmVar (Ident "n")))))) (TmArrow (TmLeft (TmTensor (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))) (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit)))) (TmRight (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b")))))))
                  ])
        `shouldNotBe` Bad ""
    it "expr" $
      check (Prog [DType (Ident "nat") (TyRec (Ident "X") (TySum TyUnit (TyVar (Ident "X"))))
                  ,DTerm (Ident "add") (TyFunc (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat")))) (TmTrace (TyVar (Ident "nat")) (TmLin (TmLin (TmArrow (TmRight (TmTensor (TmVar (Ident "x")) (TmVar (Ident "y")))) (TmLeft (TmTensor (TmTensor (TmVar (Ident "y")) (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit))) (TmVar (Ident "x"))))) (TmArrow (TmLeft (TmTensor (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))) (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "n")))))) (TmLeft (TmTensor (TmTensor (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "a")))) (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "b"))))) (TmVar (Ident "n")))))) (TmArrow (TmLeft (TmTensor (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))) (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit)))) (TmRight (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b")))))))
                  ,DTerm (Ident "one") (TyVar (Ident "nat")) (TmFold (TyVar (Ident "nat")) (TmRight (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit))))
                  ,DExpr (Ident "two") (TyVar (Ident "nat")) (ExApp (ExTerm (TmVar (Ident "add"))) (TmTensor (TmVar (Ident "one")) (TmVar (Ident "one"))))
                  ])
        `shouldNotBe` Bad ""
