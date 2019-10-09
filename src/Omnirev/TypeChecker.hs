{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Omnirev.TypeChecker(check) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Data.Map as Map

import Omnirev.AbsOmnirev


data GlobalVar
  = GVType Type
  | GVTerm Term Type
  deriving (Eq, Ord, Show, Read)

data LocalVar
  = LVInd Type
  | LVLet Type Term
  | LVRec Type Term
  deriving (Eq, Ord, Show, Read)

type Context a = Map String a
type Env = Context GlobalVar

-- https://qiita.com/HirotoShioi/items/8a6107434337b30ce457 を参考
newtype Eval a = Eval (ReaderT (Context LocalVar) (StateT Env (ExceptT String Identity)) a)
  deriving (Functor, Applicative, Monad, MonadReader (Context LocalVar), MonadState Env, MonadError String)


-- Entrypoint of type checking 
check :: Program -> String
check p = case runEval (checkProg p) Map.empty of
  Left  x -> x
  Right _ -> "Success!"

runEval :: Eval a -> Env -> Either String a
runEval (Eval m) env = runIdentity (runExceptT (evalStateT (runReaderT m Map.empty) env))

unknownVarError :: Show a => a -> Eval b
unknownVarError x = throwError $ "Variable not found: " ++ show x

nestedVarError :: Show a => a -> Eval b
nestedVarError x = throwError $ "Recursive type variable can not nest: " ++ show x

dupDefTypeError :: Show a => a -> Eval b
dupDefTypeError x = throwError $ "The variable is already defined as Type: " ++ show x

dupDefTermError :: Show a => a -> Eval b
dupDefTermError x = throwError $ "The variable is already defined as Term: " ++ show x

dupVarError :: Show a => GlobalVar -> a -> Eval b
dupVarError GVType{} = dupDefTypeError
dupVarError GVTerm{} = dupDefTermError

invalidTypeError :: Show a => a -> Eval b
invalidTypeError x = throwError $ "Invalid Type: " ++ show x

checkProg :: Program -> Eval String
checkProg (Prog []) = throwError "There is no program"
checkProg (Prog defs) = checkDefs defs

checkDefs :: [Def] -> Eval String
checkDefs []   = pure ""
checkDefs [d]  = checkDef d
checkDefs (d:ds) = (++) <$> checkDef d <*> checkDefs ds

checkDef :: Def -> Eval String
checkDef (DType (Ident s) ty) = do
  env <- get
  case Map.lookup s env of
    Nothing -> do
      ty' <- purify ty
      checkType ty'
      modify $ Map.insert s (GVType ty')
      pure ""
    Just v -> dupVarError v s
checkDef (DTerm (Ident s) ty tm) = do
  env <- get
  case Map.lookup s env of
    Nothing -> do
      ty' <- purify ty
      checkTerm tm ty'
      modify $ Map.insert s (GVTerm tm ty')
      pure ""
    Just v -> dupVarError v s

-- The `purify` function replace type variables.
purify :: Type -> Eval Type
purify TUnit            = pure TUnit
purify (TTensor t1 t2)  = do
  t1' <- purify t1
  t2' <- purify t2
  pure $ TTensor t1' t2'
purify (TSum t1 t2)     = do
  t1' <- purify t1
  t2' <- purify t2
  pure $ TSum t1' t2'
purify (TDual t)        = do
  t' <- purify t
  pure $ TDual t'
purify (TInd (Ident s) t) = do
  t' <- local (Map.insert s (LVInd t)) (purify t)
  pure $ TInd (Ident s) t'
purify (TVar (Ident s)) = do
  -- 局所変数と大域変数で名前かぶった場合は局所変数として優先的に解釈
  cxt <- ask
  case Map.lookup s cxt of
    -- 局所変数ならそのまま返す
    Just (LVInd t) -> pure (TVar (Ident s))
    -- 定義通りならここには来ないはずよね・・・
    Just v -> throwError $ "Sorry, something went wrong."
    Nothing -> do
      env <- get
      case Map.lookup s env of
        -- すでに大域変数として宣言されているなら既にpurifyされているから不要?
        Just (GVType t) -> purify t
        Just v -> dupVarError v s
        Nothing -> unknownVarError s

checkType :: Type -> Eval String
checkType TUnit            = pure ""
checkType (TTensor t1 t2)  = checkType t1 >> checkType t2
checkType (TSum t1 t2)     = checkType t1 >> checkType t2
checkType (TDual t)        = checkType t
checkType (TInd (Ident s) t) = do
  cxt <- ask
  -- ロガー的なモナドで他の局所変数を忘却している事を明示したい
  local (\c -> Map.singleton s (LVInd t)) (checkType t)
checkType (TVar (Ident s)) = do
  -- 局所変数と大域変数で名前かぶった場合は局所変数として優先的に解釈
  cxt <- ask
  -- 他の局所変数が残っている場合はエラーで返す
  case size cxt of
    0 -> do --大域変数として判定
      env <- get
      case Map.lookup s env of
        Just (GVType t) -> pure ""
        Just v          -> dupVarError v s
        Nothing         -> unknownVarError s
    1 -> pure ""
    _ -> throwError "local type context must be empty or only one."

checkTerm :: Term -> Type -> Eval String
checkTerm _ _ = throwError "under construction."
{-
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
  env <- get
  case Map.lookup s env of
    -- 定義した変数の参照回数を線形性のために丁度一回とする場合はここを変更すること
    Just (VExpr e' t') ->
      if t == t'
        then pure ""
        else invalidTypeError s
    Just v -> dupVarError v s
    Nothing -> unknownVarError s
checkExpr (EApp f e) t = do
  d <- searchCodomain f t
  checkExpr e d
checkExpr (EProj e) t = checkExpr e t
-}

{-
checkFunc :: Func -> Domain -> Codomain -> Eval String
checkFunc FId t t' =
  if t == t'
    then pure ""
    else throwError "id function must have same domain and codomain"
checkFunc (FComp f1 f2) t1 t3 = do
  t2 <- searchCodomain f1 t1
  t2' <- searchDomain f2 t3
  if t2 == t2'
    then pure ""
    else throwError $ "composite function must have same codomain of left side function and domain of right side function: " ++ show t2 ++ "\n" ++ show t2'
checkFunc (FTensor f1 f2) (TTensor t1 t2) (TTensor t3 t4) =
  checkFunc f1 t1 t3 >> checkFunc f2 t2 t4
checkFunc FTensor{} _ _ = throwError "tensor function must have tensor type in domain and codomain"
checkFunc FTensUnit (TTensor TUnit t) t' =
  if t == t'
    then pure ""
    else invalidFunctionError "unit*"
checkFunc FTensAssoc (TTensor t1 (TTensor t2 t3)) (TTensor (TTensor t1' t2') t3') =
  if t1 == t1' && t2 == t2' && t3 == t3'
    then pure ""
    else invalidFunctionError "assoc*"
checkFunc FTensSym (TTensor t1 t2) (TTensor t2' t1') =
  if t1 == t1' && t2 == t2'
    then pure ""
    else invalidFunctionError "sym*"
checkFunc (FSum f1 f2) (TSum t1 t2) (TSum t3 t4) =
  checkFunc f1 t1 t3 >> checkFunc f2 t2 t4
checkFunc FSum{} _ _ = throwError "sum function must have sum type in domain and codomain"
checkFunc FSumAssoc (TSum t1 (TSum t2 t3)) (TSum (TSum t1' t2') t3') =
  if t1 == t1' && t2 == t2' && t3 == t3'
    then pure ""
    else invalidFunctionError "assoc+"
checkFunc FSumSym (TSum t1 t2) (TSum t2' t1') =
  if t1 == t1' && t2 == t2'
    then pure ""
    else invalidFunctionError "sym+"
checkFunc FDistrib (TTensor (TSum t1 t2) t3) (TSum (TTensor t1' t3') (TTensor t2' t3'')) =
  if t1 == t1' && t2 == t2' && t3 == t3' && t3' == t3''
    then pure ""
    else invalidFunctionError "distrib"
checkFunc (FEval t) (TTensor t' (TStar t'')) TUnit = do
  t''' <- purify t
  if t''' == t' && t' == t''
    then pure ""
    else invalidFunctionError "eval"
checkFunc (FDagger f) cod dom = checkFunc f dom cod
checkFunc (FVar (Ident s)) dom cod = do
  env <- get
  case Map.lookup s env of
    Just (VFunc f' dom' cod') ->
      if dom == dom' && cod == cod'
        then pure ""
        else invalidFunctionError s
    Just v -> dupVarError v s
    Nothing        -> unknownVarError s
checkFunc _ _ _ = throwError "Invalid domain or codomain"
-}

{-
searchCodomain :: Func -> Domain -> Eval Codomain
searchCodomain FId d = pure d
searchCodomain (FComp f1 f2) d1 = do
  d2 <- searchCodomain f1 d1
  searchCodomain f2 d2
searchCodomain (FTensor f1 f2) (TTensor d1 d2) = do
  c1 <- searchCodomain f1 d1
  c2 <- searchCodomain f2 d2
  pure $ TTensor c1 c2
searchCodomain FTensUnit (TTensor TUnit d) = pure d
searchCodomain FTensAssoc (TTensor d1 (TTensor d2 d3)) = pure $ TTensor (TTensor d1 d2) d3
searchCodomain FTensSym (TTensor d1 d2) = pure $ TTensor d2 d1
searchCodomain (FSum f1 f2) (TSum d1 d2) = do
  c1 <- searchCodomain f1 d1
  c2 <- searchCodomain f2 d2
  pure $ TSum c1 c2
searchCodomain FSumAssoc (TSum d1 (TSum d2 d3)) = pure $ TSum (TSum d1 d2) d3
searchCodomain FSumSym (TSum d1 d2) = pure $ TSum d2 d1
searchCodomain FDistrib (TTensor (TSum d1 d2) d3) = pure $ TSum (TTensor d1 d3) (TTensor d2 d3)
searchCodomain (FEval t) (TTensor d (TStar d')) = do
  t' <- purify t
  if t' == d && d == d'
    then pure TUnit
    else invalidFunctionError "eval"
searchCodomain (FDagger f) d = searchDomain f d
searchCodomain (FVar (Ident s)) d = do
  env <- get
  case Map.lookup s env of
    Just (VFunc f d' c) ->
      if d == d'
        then purify c
        else invalidFunctionError s
    Just v -> dupVarError v s
    Nothing -> unknownVarError s
searchCodomain _ _ = throwError "Invalid domain or codomain"
-}

{-
searchDomain :: Func -> Codomain -> Eval Domain
searchDomain FId c = pure c
searchDomain (FComp f1 f2) c3 = do
  d2 <- searchDomain f2 c3
  searchDomain f1 d2
searchDomain (FTensor f1 f2) (TTensor c1 c2) = do
  d1 <- searchDomain f1 c1
  d2 <- searchDomain f2 c2
  pure $ TTensor d1 d2
searchDomain FTensUnit c = pure $ TTensor TUnit c
searchDomain FTensAssoc (TTensor (TTensor c1 c2) c3) = pure $ TTensor c1 (TTensor c2 c3)
searchDomain FTensSym (TTensor c1 c2) = pure $ TTensor c2 c1
searchDomain (FSum f1 f2) (TSum c1 c2) = do
  d1 <- searchDomain f1 c1
  d2 <- searchDomain f2 c2
  pure $ TSum d1 d2
searchDomain FSumAssoc (TSum (TSum c1 c2) c3) = pure $ TSum c1 (TSum c2 c3)
searchDomain FSumSym (TSum c1 c2) = pure $ TSum c2 c1
searchDomain FDistrib (TSum (TTensor c1 c3) (TTensor c2 c3')) =
  if c3 == c3'
    then pure $ TTensor (TSum c1 c2) c3
    else invalidFunctionError "distrib"
searchDomain (FEval t) TUnit = do
  t' <- purify t
  pure $ TTensor t' (TStar t')
searchDomain (FDagger f) c = searchDomain f c
searchDomain (FVar (Ident s)) c = do
  env <- get
  case Map.lookup s env of
    Just (VFunc f d c') ->
      if c == c'
        then purify d
        else invalidFunctionError s
    Just v -> dupVarError v s
    Nothing -> unknownVarError s
searchDomain _ _ = throwError "Invalid domain or codomain"
-}