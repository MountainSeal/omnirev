module PrintOmnirevSpec where

import Omnirev.AbsOmnirev
import Omnirev.PrintOmnirev
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "printTree" $ do
    it "" $ 1 `shouldBe` 1