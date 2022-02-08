module Omnirev.Env where

import Data.Map as M
import Omnirev.AbsOmnirev

type Env a = M.Map String a

data Alias
  = AType Type
  | ATerm Term Type
  | AExpr Expr Type
  | AVar-- 再帰型の束縛変数（フレッシュでない変数名）
  deriving (Eq, Ord, Show, Read)

data Result
  = RType Type
  | RTerm Term
  | RExpr Expr
  deriving (Eq, Ord, Show, Read)