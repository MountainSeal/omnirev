-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module Omnirev.AbsOmnirev where

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read)

data Program = Prog [Def]
  deriving (Eq, Ord, Show, Read)

data Def
    = DType Ident Type | DExpr Ident Type Expr | DTerm Ident Type Term
  deriving (Eq, Ord, Show, Read)

data Type
    = TyVar Ident
    | TyUnit
    | TySum Type Type
    | TyTensor Type Type
    | TyFunc Type Type
    | TyRec Ident Type
  deriving (Eq, Ord, Show, Read)

data Term
    = TmVar Ident
    | TmUnit
    | TmLeft Term
    | TmRight Term
    | TmTensor Term Term
    | TmArrow Term Term
    | TmFold Type Term
    | TmLin Term Term
    | TmTrace Type Term
    | TmComp Term Term
    | TmFlip Term
    | TmEmpty
    | TmId
  deriving (Eq, Ord, Show, Read)

data Expr = ExTerm Term | ExApp Expr Term
  deriving (Eq, Ord, Show, Read)