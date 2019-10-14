module TypeCheckerSpec where

import Omnirev.AbsOmnirev
import Omnirev.TypeChecker as TC
import Omnirev.ErrM as Err
import Test.Hspec
import Data.Map as Map

spec :: Spec
spec = do
  describe "check" $ do
    it "整形な型の型付けに成功すること" $
      check (Prog [ DType (Ident "alias") TUnit
                  , DType (Ident "qubit") (TSum TUnit TUnit)
                  , DType (Ident "tens") (TTensor (TSum TUnit TUnit) (TSum TUnit TUnit))
                  , DType (Ident "star") (TTensor (TDual (TTensor (TSum TUnit TUnit) (TSum TUnit TUnit))) (TTensor (TSum TUnit TUnit) (TSum TUnit TUnit)))
                  , DType (Ident "nat") (TInd (Ident "x") (TSum TUnit (TVar (Ident "x"))))
                  , DType (Ident "tvar") (TVar (Ident "qubit"))
                  , DType (Ident "natlist") (TInd (Ident "x") (TSum TUnit (TTensor (TVar (Ident "nat")) (TVar (Ident "x")))))
                  ])
      `shouldBe` "Success!"
    it "入れ子になった再帰型の型付けに失敗すること" $
      check (Prog [ DType (Ident "nested") (TInd (Ident "x") (TSum TUnit (TInd (Ident "y") (TSum (TVar (Ident "x")) (TVar (Ident "y"))))))
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
  describe "purify" $ do
    it "simple" $
      runEval (purify (TVar (Ident "qubit"))) (Map.fromList [("qubit", (VType (TSum TUnit TUnit)))])
        `shouldBe` Right (TSum TUnit TUnit)
    it "nested" $
      runEval (purify (TTensor TUnit (TVar (Ident "qubit")))) (Map.fromList [("qubit", (VType (TSum TUnit TUnit)))])
        `shouldBe` Right (TTensor TUnit (TSum TUnit TUnit))
    
