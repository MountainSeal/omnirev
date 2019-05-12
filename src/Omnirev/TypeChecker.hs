{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Omnirev.TypeChecker where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State

import Data.Map as Map

import Omnirev.AbsOmnirev


type Context a = Map String a

type Domain   = Type
type Codomain = Type

data Var
  = VType Type
  | VFunc Func Domain Codomain
  | VExpr Expr Type
  deriving (Eq, Ord, Show, Read)

-- https://qiita.com/HirotoShioi/items/8a6107434337b30ce457 を参考
newtype Eval a = Eval (StateT (Context Var) (ExceptT String Identity) a)
  deriving (Functor, Applicative, Monad, MonadState (Context Var), MonadError String)


-- Entrypoint of type checking 
check :: Program -> String
check p = case runEval (checkProg p) Map.empty of
  Left  x -> x
  Right _ -> "Success!"

runEval :: Eval a -> Context Var -> Either String a
runEval (Eval m) cxt = runIdentity (runExceptT (evalStateT m cxt))

unknownVarError :: Show a => a -> Eval b
unknownVarError x = throwError $ "Variable not found: " ++ show x

conflictVarError :: Show a => a -> Eval b
conflictVarError x = throwError $ "Variable is already exists: " ++ show x

dupDefTypeError :: Show a => a -> Eval b
dupDefTypeError x = throwError $ "The variable is already defined as Type: " ++ show x

dupDefFuncError :: Show a => a -> Eval b
dupDefFuncError x = throwError $ "The variable is already defined as Function: " ++ show x

dupDefExprError :: Show a => a -> Eval b
dupDefExprError x = throwError $ "The variable is already defined as Expression: " ++ show x

dupVarError :: Show a => Var -> a -> Eval b
dupVarError VType{} = dupDefTypeError
dupVarError VExpr{} = dupDefExprError
dupVarError VFunc{} = dupDefFuncError

invalidTypeError :: Show a => a -> Eval b
invalidTypeError x = throwError $ "Invalid Type: " ++ show x

invalidFunctionError :: Show a => a -> Eval b
invalidFunctionError x = throwError $ "Invalid domain or codomain: " ++ show x

checkProg :: Program -> Eval String
checkProg (Prog []) = throwError "There is no program"
checkProg (Prog defs) = checkDefs defs

checkDefs :: [Def] -> Eval String
checkDefs []   = pure ""
checkDefs [d]  = checkDef d
checkDefs (d:ds) = (++) <$> checkDef d <*> checkDefs ds

checkDef :: Def -> Eval String
checkDef (DType (Ident s) t) = do
  cxt <- get
  case Map.lookup s cxt of
    Nothing -> do
      checkType t
      modify (Map.insert s (VType t))
      pure ""
    Just v -> dupVarError v s
checkDef (DFunc (Ident s) dom cod f) = do
  cxt <- get
  case Map.lookup s cxt of
    Nothing -> do
      d <- purify dom
      c <- purify cod
      checkFunc f d c
      modify (Map.insert s (VFunc f d c))
      pure ""
    Just v -> dupVarError v s
checkDef (DExpr (Ident s) t e) = do
  cxt <- get
  case Map.lookup s cxt of
    Nothing -> do
      checkExpr e t
      modify (Map.insert s (VExpr e t))
      pure ""
    Just v -> dupVarError v s

-- The `purify` function replace type variables.
purify :: Type -> Eval Type
purify TUnit            = pure TUnit
purify (TTensor t1 t2)  = purify t1 >> purify t2
purify (TSum t1 t2)     = purify t1 >> purify t2
purify (TStar t)        = purify t
purify (TVar (Ident s)) = do
  cxt <- get
  case Map.lookup s cxt of
    Just (VType t) -> pure t
    Just v -> dupVarError v s
    Nothing        -> unknownVarError s

checkType :: Type -> Eval String
checkType TUnit            = pure ""
checkType (TTensor t1 t2)  = checkType t1 >> checkType t2
checkType (TSum t1 t2)     = checkType t1 >> checkType t2
checkType (TStar t)        = checkType t
checkType (TVar (Ident s)) = do
  cxt <- get
  case Map.lookup s cxt of
    Just (VType t) -> pure ""
    Just v -> dupVarError v s
    Nothing        -> unknownVarError s

checkExpr :: Expr -> Type -> Eval String
checkExpr EUnit TUnit = pure ""
checkExpr EUnit _     = throwError "() must be typed as unit"
checkExpr (ETensor e1 e2) (TTensor t1 t2) = checkExpr e1 t1 >> checkExpr e2 t2
checkExpr ETensor{} _                     = throwError "tensor must typed as tensor type"
checkExpr (ESum e1 e2) (TSum t1 t2) = checkExpr e1 t1 >> checkExpr e2 t2
checkExpr ESum{} _                  = throwError "sum must typed as sum type"
checkExpr (EStar e) (TStar t) = checkExpr e t
checkExpr EStar{} _           = throwError "dual must typed as dual type"
checkExpr (EVar (Ident s)) t = do
  cxt <- get
  case Map.lookup s cxt of
    -- 定義した変数の参照回数を線形性のために丁度一回とする場合はここを変更すること
    Just (VExpr e' t') ->
      if t == t'
        then pure ""
        else invalidTypeError s
    Just v -> dupVarError v s
    Nothing -> unknownVarError s

checkFunc :: Func -> Domain -> Codomain -> Eval String
checkFunc FId dom cod =
  if dom == cod
    then pure ""
    else throwError "id function must have same domain and codomain"
checkFunc (FComp f1 f2) dom cod = do
  cod' <- searchCodomain (FComp f1 f2) dom
  if cod == cod'
    then pure ""
    else throwError "composite function must have same codomain of left side function and domain of right side function"
checkFunc (FTensor f1 f2) (TTensor t1 t2) (TTensor t3 t4) =
  checkFunc f1 t1 t3 >> checkFunc f2 t2 t4
checkFunc FTensor{} _ _ = throwError "tensor function must have tensor type in domain and codomain"
checkFunc FTensUnit (TTensor TUnit t) t' =
  if t == t'
    then pure ""
    else invalidFunctionError "unit_*"
checkFunc FTensAssoc (TTensor t1 (TTensor t2 t3)) (TTensor (TTensor t1' t2') t3') =
  if t1 == t1' && t2 == t2' && t3 == t3'
    then pure ""
    else invalidFunctionError "assoc_*"
checkFunc FTensSym (TTensor t1 t2) (TTensor t2' t1') =
  if t1 == t1' && t2 == t2'
    then pure ""
    else invalidFunctionError "sym_*"
checkFunc (FSum f1 f2) (TSum t1 t2) (TSum t3 t4) =
  checkFunc f1 t1 t3 >> checkFunc f2 t2 t4
checkFunc FSum{} _ _ = throwError "sum function must have sum type in domain and codomain"
checkFunc FSumAssoc (TSum t1 (TSum t2 t3)) (TSum (TSum t1' t2') t3') =
  if t1 == t1' && t2 == t2' && t3 == t3'
    then pure ""
    else invalidFunctionError "assoc_+"
checkFunc FSumSym (TSum t1 t2) (TSum t2' t1') =
  if t1 == t1' && t2 == t2'
    then pure ""
    else invalidFunctionError "sym_+"
checkFunc FDistrib (TTensor (TSum t1 t2) t3) (TSum (TTensor t1' t3') (TTensor t2' t3'')) =
  if t1 == t1' && t2 == t2' && t3 == t3' && t3' == t3''
    then pure ""
    else invalidFunctionError "distrib"
checkFunc (FEval t) (TTensor t' (TStar t'')) TUnit =
  if t == t' && t' == t''
    then pure ""
    else invalidFunctionError "eval"
checkFunc (FDagger f) cod dom = checkFunc f dom cod
checkFunc (FVar (Ident s)) dom cod = do
  cxt <- get
  case Map.lookup s cxt of
    Just (VFunc f' dom' cod') ->
      if dom == dom' && cod == cod'
        then pure ""
        else invalidFunctionError s
    Just v -> dupVarError v s
    Nothing        -> unknownVarError s
checkFunc FShift{} dom cod =
  if dom == cod
    then pure ""
    else throwError "shift function must have same domain and codomain"
checkFunc _ _ _ = throwError "Invalid domain or codomain"

searchCodomain :: Func -> Domain -> Eval Type
searchCodomain FId t = pure t
searchCodomain (FComp f1 f2) t1 = do
  t2 <- searchCodomain f1 t1
  searchCodomain f2 t2
searchCodomain (FTensor f1 f2) (TTensor t1 t2) = searchCodomain f1 t1 >> searchCodomain f2 t2
searchCodomain FTensUnit (TTensor TUnit t) = pure t
searchCodomain FTensAssoc (TTensor t1 (TTensor t2 t3)) = pure (TTensor (TTensor t1 t2) t3)
searchCodomain FTensSym (TTensor t1 t2) = pure (TTensor t2 t1)
searchCodomain (FSum f1 f2) (TSum t1 t2) = searchCodomain f1 t1 >> searchCodomain f2 t2
searchCodomain FSumAssoc (TSum t1 (TSum t2 t3)) = pure (TSum (TSum t1 t2) t3)
searchCodomain FSumSym (TSum t1 t2) = pure (TSum t2 t1)
searchCodomain FDistrib (TTensor (TSum t1 t2) t3) = pure (TSum (TTensor t1 t3) (TTensor t2 t3))
searchCodomain (FEval t) (TTensor t' (TStar t'')) =
  if t == t' && t' == t''
    then pure TUnit
    else invalidFunctionError "eval"
searchCodomain (FDagger f) t = searchDomain f t
searchCodomain (FVar (Ident s)) t = do
  cxt <- get
  case Map.lookup s cxt of
    Just (VFunc f d c) ->
      if t == d
        then pure c
        else invalidFunctionError s
    Just v -> dupVarError v s
    Nothing -> unknownVarError s
searchCodomain FShift{} t = pure t
searchCodomain _ _ = throwError "Invalid domain or codomain"

searchDomain :: Func -> Codomain -> Eval Type
searchDomain FId t = pure t
searchDomain (FComp f1 f2) t3 = do
  t2 <- searchDomain f2 t3
  searchDomain f1 t2
searchDomain (FTensor f1 f2) (TTensor t1 t2) = searchDomain f1 t1 >> searchDomain f2 t2
searchDomain FTensUnit t = pure (TTensor TUnit t)
searchDomain FTensAssoc (TTensor (TTensor t1 t2) t3) = pure (TTensor t1 (TTensor t2 t3))
searchDomain FTensSym (TTensor t1 t2) = pure (TTensor t2 t1)
searchDomain (FSum f1 f2) (TSum t1 t2) = searchDomain f1 t1 >> searchDomain f2 t2
searchDomain FSumAssoc (TSum (TSum t1 t2) t3) = pure (TSum t1 (TSum t2 t3))
searchDomain FSumSym (TSum t1 t2) = pure (TSum t2 t1)
searchDomain FDistrib (TSum (TTensor t1 t3) (TTensor t2 t3')) =
  if t3 == t3'
    then pure (TTensor (TSum t1 t2) t3)
    else invalidFunctionError "distrib"
searchDomain (FEval t) TUnit = pure (TTensor t (TStar t))
searchDomain (FDagger f) t = searchDomain f t
searchDomain (FVar (Ident s)) t = do
  cxt <- get
  case Map.lookup s cxt of
    Just (VFunc f d c) ->
      if t == c
        then pure d
        else invalidFunctionError s
    Just v -> dupVarError v s
    Nothing -> unknownVarError s
searchDomain FShift{} t = pure t
searchDomain _ _ = throwError "Invalid domain or codomain"