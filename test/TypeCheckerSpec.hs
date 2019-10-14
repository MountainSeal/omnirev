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
  
  describe "subst" $ do
    it "整形な再帰型の再帰変数が「正しく」置換えられること" $
      runEval (subst (TSum TUnit (TVar (Ident "x")))
                     (TInd (Ident "x") (TSum TUnit (TVar (Ident "x"))))
                     (Ident "x")
              )
              (Map.fromList [("nat", GVType (TInd (Ident "x") (TSum TUnit (TVar (Ident "x")))))])
      `shouldBe` Right (TSum TUnit (TInd (Ident "x") (TSum TUnit (TVar (Ident "x")))))
    
    it "入れ子再帰で同じ再帰変数を使っている場合は内側の再帰型の再帰変数として置換えあられること" $
      runEval (subst (TSum TUnit (TTensor (TInd (Ident "x") (TSum TUnit (TVar (Ident "x")))) (TVar (Ident "x"))))
                     (TInd (Ident "x") (TSum TUnit (TTensor (TInd (Ident "x") (TSum TUnit (TVar (Ident "x")))) (TVar (Ident "x")))))
                     (Ident "x")
              )
              Map.empty
      `shouldBe`
              Right (TSum TUnit (TTensor (TInd (Ident "x") (TSum TUnit (TVar (Ident "x")))) (TInd (Ident "x") (TSum TUnit (TTensor (TInd (Ident "x") (TSum TUnit (TVar (Ident "x")))) (TVar (Ident "x")))))))