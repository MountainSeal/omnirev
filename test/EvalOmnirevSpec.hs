module EvalOmnirevSpec where

import qualified Data.Map as M
import Omnirev.AbsOmnirev
import Omnirev.CheckOmnirev
import Omnirev.EvalOmnirev
import Omnirev.ErrM
import Omnirev.Env
import Test.Hspec
import Prelude hiding ((++), (*), id, not, (&&), (**), (*>), (||))
import Utils


idl :: [Ident]
env :: Env Alias
(idl, env) = case check program' of
  Ok (res, logs) -> res
  Bad logs -> ([], M.empty)

strs :: [String]
strs = map (\(Ident s) -> s) idl


hoge :: [Expr]
hoge = map g $ filter f $ map (`M.lookup` env) strs
  where
  f :: Maybe Alias -> Bool
  f mba = case mba of
    Nothing -> False
    Just AType{} -> False
    Just ATerm{} -> False
    Just AExpr{} -> True
    Just AVar -> False
  g al = case al of
    Just (AExpr ex ty) -> ex

terms :: [Either String Term]
terms = map (\ex -> evalEval (evalExpr ex) env) hoge

spec :: Spec
spec = do
  describe "eval" $ do
    it "term" $ head (tail $ tail terms) `shouldBe` Left ""
    -- it "term" $ eval (idl, env) `shouldBe` Bad ""
    -- it "term" $ (idl, env) `shouldBe` ([], M.empty)