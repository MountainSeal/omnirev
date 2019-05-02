

module Omnirev.AbsOmnirev where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program = Prog [Def]
  deriving (Eq, Ord, Show, Read)

data Def
    = DType Ident Type
    | DFunc Ident Type Type Func
    | DExpr Ident Type Expr
  deriving (Eq, Ord, Show, Read)

data Expr
    = EUnit
    | ETensor Expr Expr
    | ESum Expr Expr
    | EStar Expr
    | EVar Ident
    | EApp Func Expr
    | EProj Expr
  deriving (Eq, Ord, Show, Read)

data Type
    = TUnit
    | TTensor Type Type
    | TSum Type Type
    | TStar Type
    | TVar Ident
  deriving (Eq, Ord, Show, Read)

data Func
    = FId
    | FComp Func Func
    | FTensor Func Func
    | FTensUnit
    | FTensAssoc
    | FTensSym
    | FSum Func Func
    | FSumAssoc
    | FSumSym
    | FDistrib
    | FEval Type
    | FDagger Func
    | FVar Ident
    | FShift Integer
  deriving (Eq, Ord, Show, Read)

