module EvalOmnirevSpec where

import Omnirev.AbsOmnirev
import Omnirev.EvalOmnirev
import Omnirev.ErrM as Err
import Test.Hspec
import Data.Map as Map

spec :: Spec
spec = do
  describe "downOpp" $ do
    it "twoOpp" $
      downOpp (TmOpp (TmOpp TmUnit))
        `shouldBe`
        TmUnit
    it "LinOpp" $
      downOpp (TmOpp (TmLin TmUnit TmUnit))
        `shouldBe`
        TmLin (TmOpp TmUnit) (TmOpp TmUnit)
    it "LROpp" $
      downOpp (TmOpp (TmLeft (TmRight TmUnit)))
        `shouldBe` 
        TmLeft (TmRight (TmOpp TmUnit))
    it "TensOpp" $
      downOpp (TmOpp (TmTensor (TmLeft TmUnit) (TmOpp (TmRight TmUnit))))
        `shouldBe`
        TmTensor (TmLeft (TmOpp TmUnit)) (TmRight TmUnit)
    it "ArrOpp" $
      downOpp (TmOpp (TmLin (TmArrow (TmLeft TmUnit) (TmLin (TmLeft TmUnit) (TmRight TmUnit))) (TmArrow (TmRight TmUnit) (TmLin (TmLeft TmUnit) (TmOpp (TmRight TmUnit))))))
        `shouldBe`
        TmLin (TmArrow (TmLeft (TmOpp TmUnit)) (TmLin (TmLeft (TmOpp TmUnit)) (TmRight (TmOpp TmUnit)))) (TmArrow (TmRight (TmOpp TmUnit)) (TmLin (TmLeft (TmOpp TmUnit)) (TmRight TmUnit)))
    it "FoldOpp" $
      downOpp (TmOpp (TmFold (TyVar (Ident "N")) (TmRight (TmFold (TyVar (Ident "N")) (TmLeft TmUnit)))))
        `shouldBe`
        TmFold (TyVar (Ident "N")) (TmRight (TmFold (TyVar (Ident "N")) (TmLeft (TmOpp TmUnit))))
  describe "upLin" $ do
    it "lru" $
      upLin (TmLeft (TmRight (TmLin TmUnit TmUnit)))
        `shouldBe`
        TmLin (TmLeft (TmRight TmUnit)) (TmLeft (TmRight TmUnit))
    it "tensLinL" $
      upLin (TmTensor (TmLin (TmLeft TmUnit) (TmRight TmUnit)) (TmLeft TmUnit))
        `shouldBe`
        TmLin (TmTensor (TmLeft TmUnit) (TmLeft TmUnit)) (TmTensor (TmRight TmUnit) (TmLeft TmUnit))
    it "tensLinR" $
      upLin (TmTensor (TmRight TmUnit) (TmLeft (TmLin TmUnit TmUnit)))
        `shouldBe`
        TmLin (TmTensor (TmRight TmUnit) (TmLeft TmUnit)) (TmTensor (TmRight TmUnit) (TmLeft TmUnit))
    it "tensLinLR" $
      upLin (TmTensor (TmLin (TmLeft TmUnit) (TmRight TmUnit)) (TmLin (TmLeft TmUnit) (TmRight TmUnit)))
        `shouldBe`
        TmLin (TmLin (TmTensor (TmLeft TmUnit) (TmLeft TmUnit)) (TmTensor (TmLeft TmUnit) (TmRight TmUnit))) (TmLin (TmTensor (TmRight TmUnit) (TmLeft TmUnit)) (TmTensor (TmRight TmUnit) (TmRight TmUnit)))
    it "foldLin" $
      upLin (TmFold (TyVar (Ident "N")) (TmLin (TmLeft TmUnit) (TmRight (TmFold (TyVar (Ident "N")) (TmLeft TmUnit)))))
        `shouldBe`
        TmLin (TmFold (TyVar (Ident "N")) (TmLeft TmUnit)) (TmFold (TyVar (Ident "N")) (TmRight (TmFold (TyVar (Ident "N")) (TmLeft TmUnit))))
    it "oppLin" $
      upLin (TmOpp (TmLin (TmVar (Ident "u")) (TmVar (Ident "u"))))
        `shouldBe`
        TmLin (TmOpp (TmVar (Ident "u"))) (TmOpp (TmVar (Ident "u")))
  describe "representative" $ do
    it "tensOffset" $
      repr (TmLin (TmTensor (TmLeft (TmOpp TmUnit)) (TmRight TmUnit)) (TmLin (TmOpp (TmTensor (TmLin (TmLeft (TmOpp TmUnit)) (TmLeft (TmOpp TmUnit))) (TmRight TmUnit))) (TmOpp (TmTensor (TmLeft (TmOpp TmUnit)) (TmLin (TmRight TmUnit) (TmRight TmUnit))))))
        `shouldBe`
        TmNull

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
        `shouldBe` "Success!"
    it "term" $
      check (Prog [DType (Ident "qubit") (TySum TyUnit TyUnit)
                  ,DTerm (Ident "not") (TyFunc (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TmLin (TmArrow (TmLeft TmUnit) (TmRight TmUnit)) (TmArrow (TmRight TmUnit) (TmLeft TmUnit)))
                  ,DTerm (Ident "cnot") (TyFunc (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))) (TmLin (TmLin (TmLin (TmArrow (TmTensor (TmLeft TmUnit) (TmLeft TmUnit)) (TmTensor (TmLeft TmUnit) (TmLeft TmUnit))) (TmArrow (TmTensor (TmLeft TmUnit) (TmRight TmUnit)) (TmTensor (TmLeft TmUnit) (TmRight TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmLeft TmUnit)) (TmTensor (TmRight TmUnit) (TmRight TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmRight TmUnit)) (TmTensor (TmRight TmUnit) (TmLeft TmUnit))))
                  -- ,DExpr (Ident "cnot_cnot") (TyFunc (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))) (ExComp (ExTerm (TmVar (Ident "cnot"))) (ExTerm (TmVar (Ident "cnot"))))
                  ,DType (Ident "nat") (TyRec (Ident "X") (TySum TyUnit (TyVar (Ident "X"))))
                  -- ,DExpr (Ident "add") (TyFunc (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat")))) (ExTrace (TmLin (TmLin (TmArrow (TmTensor (TmVar (Ident "x")) (TmVar (Ident "y"))) (TmLabel (Ident "iter") (TmTensor (TmTensor (TmVar (Ident "y")) (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit))) (TmVar (Ident "x"))))) (TmArrow (TmLabel (Ident "iter") (TmTensor (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))) (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit)))) (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))))) (TmArrow (TmLabel (Ident "iter") (TmTensor (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))) (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "n")))))) (TmLabel (Ident "iter") (TmTensor (TmTensor (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "a")))) (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "b"))))) (TmVar (Ident "n")))))) (Ident "iter") (TyTensor (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (TyVar (Ident "nat"))))
                  -- ,DExpr (Ident "test") (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (ExApp (ExTerm (TmVar (Ident "add"))) (TmTensor (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit)) (TmFold (TyVar (Ident "nat")) (TmRight (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit))))))
                  ])
        `shouldBe` "Success!"
    -- it "expr" $
    --   check (Prog [DType (Ident "qubit") (TySum TyUnit TyUnit)
    --               ,DExpr (Ident "cnot_apply") (TyFunc (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))) (ExComp (ExTerm (TmVar (Ident "cnot"))) (ExTerm (TmVar (Ident "cnot"))))
    --               ,DType (Ident "nat") (TyRec (Ident "X") (TySum TyUnit (TyVar (Ident "X"))))
    --               -- ,DExpr (Ident "add") (TyFunc (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat")))) (ExTrace (TmLin (TmLin (TmArrow (TmTensor (TmVar (Ident "x")) (TmVar (Ident "y"))) (TmLabel (Ident "iter") (TmTensor (TmTensor (TmVar (Ident "y")) (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit))) (TmVar (Ident "x"))))) (TmArrow (TmLabel (Ident "iter") (TmTensor (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))) (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit)))) (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))))) (TmArrow (TmLabel (Ident "iter") (TmTensor (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))) (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "n")))))) (TmLabel (Ident "iter") (TmTensor (TmTensor (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "a")))) (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "b"))))) (TmVar (Ident "n")))))) (Ident "iter") (TyTensor (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (TyVar (Ident "nat"))))
    --               -- ,DExpr (Ident "test") (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (ExApp (ExTerm (TmVar (Ident "add"))) (TmTensor (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit)) (TmFold (TyVar (Ident "nat")) (TmRight (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit))))))
    --               ])
    --     `shouldBe` "Success!"
  -- describe "inference" $ do
  --   it "infer" $
  --   it "exfer" $