{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Author: MountainSeal
module Omnirev.EvalOmnirev(check) where


import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.List
import Omnirev.AbsOmnirev


data Alias
  = AType Type
  | AExpr Expr Type
  deriving (Eq, Ord, Show, Read)

newtype Label = Label Type
  deriving (Eq, Ord, Show, Read)

type Env a = Map.Map String a

-- https://qiita.com/HirotoShioi/items/8a6107434337b30ce457 を参考
newtype Check a = Check (ReaderT (Env Label) (StateT (Env Alias) (ExceptT String Identity)) a)
  deriving (Functor, Applicative, Monad, MonadReader (Env Label), MonadState (Env Alias), MonadError String)


runCheck :: Check a -> Env Alias -> Either String a
runCheck (Check m) env = runIdentity (runExceptT (evalStateT (runReaderT m Map.empty) env))

-- Entrypoint of type checking
check :: Program -> String
check p = case runCheck (checkProg p) Map.empty of
  Left  x -> x
  Right _ -> "Success!"

defNotFoundError :: Show a => a -> Check b
defNotFoundError x = throwError $ "The alias not found: " ++ show x

dupDefTypeError :: Show a => a -> Check b
dupDefTypeError x = throwError $ "The alias is already defined as Type: " ++ show x

dupDefExprError :: Show a => a -> Check b
dupDefExprError x = throwError $ "The alias is already defined as Expression: " ++ show x

dupDefError :: Show a => Alias -> a -> Check b
dupDefError AType{} = dupDefTypeError
dupDefError AExpr{} = dupDefExprError

typeCheckForTermError :: Show a => a -> Check b
typeCheckForTermError x = throwError $ "Type check failed: " ++ show x

labelNotFoundError :: Show a => a -> Check b
labelNotFoundError x = typeCheckForTermError $ "Recursion label" ++ show x ++ "not found."

checkProg :: Program -> Check String
checkProg (Prog []) = throwError "There is no program"
checkProg (Prog defs) = checkDefs defs

checkDefs :: [Def] -> Check String
checkDefs []   = pure ""
checkDefs [d]  = checkDef d
checkDefs (d:ds) = (++) <$> checkDef d <*> checkDefs ds

checkDef :: Def -> Check String
checkDef (DType (Ident s) ty) = do
  env <- get
  case Map.lookup s env of
    Nothing -> do
      checkType [] ty
      modify $ Map.insert s (AType ty)
      pure ""
    Just v -> dupDefError v s
checkDef (DTerm (Ident s) ty ex) = do
  env <- get
  case Map.lookup s env of
    Nothing -> do
      checkExpr ex ty
      modify $ Map.insert s (AExpr ex ty)
      pure ""
    Just v -> dupDefError v s

-- check type formation (see Type Formation rules)
checkType :: [Ident] -> Type -> Check String
checkType cxt (TyVar (Ident s)) = if Ident s `elem` cxt
  then pure ""
  else do -- Aliasのほうの変数だと解釈する
    env <- get
    case Map.lookup s env of
      Just AType{} -> pure "" -- Aliasは検査済なので何もしない
      Just AExpr{} -> dupDefExprError s
      Nothing      -> defNotFoundError s
checkType cxt TyUnit =
  pure ""
checkType cxt (TySum t1 t2) =
  checkType cxt t1 >> checkType cxt t2
checkType cxt (TyTensor t1 t2) =
  checkType cxt t1 >> checkType cxt t2
checkType cxt (TyFunc t1 t2) =
  checkType cxt t1 >> checkType cxt t2
checkType cxt (TyRec x t) =
  checkType (x:cxt) t

subst :: Type -> Ident -> Type -> Type
subst (TyVar y)        x s = if y == x then s else TyVar y
subst TyUnit           x s = TyUnit
subst (TySum t1 t2)    x s = TySum (subst t1 x s) (subst t2 x s)
subst (TyTensor t1 t2) x s = TyTensor (subst t1 x s) (subst t2 x s)
subst (TyFunc t1 t2)   x s = TyFunc (subst t1 x s) (subst t2 x s)
subst (TyRec y t)      x s = TyRec y (subst t x s)

type Context = [(Term, Type)]
checkTerm :: Context -> (Term, Type) -> Check Context
-- variable
checkTerm cxt (TmVar i, ty) =
  case lookup (TmVar i) cxt of
    Just ty' -> if ty == ty'
      then pure $ delete (TmVar i, ty') cxt
      else typeCheckForTermError "same variable but different type."
    Nothing -> typeCheckForTermError "not found variable."
-- exchange
checkTerm ((TmVar i,ty):cxt) (um, uy) =
  checkTerm (cxt ++ [((TmVar i),ty)]) (um, uy)
-- unit_l
checkTerm ((TmUnit, TyUnit):cxt) (tm, ty) =
  checkTerm cxt (tm, ty)
-- inl_l
checkTerm ((TmLeft tm1, TySum ty1 ty2):cxt) (tm, ty) =
  checkTerm ((tm1, ty1):cxt) (tm, ty)
  -- inr_l
checkTerm ((TmRight tm2, TySum ty1 ty2):cxt) (tm, ty) =
  checkTerm ((tm2, ty2):cxt) (tm, ty)
-- tensor_l
checkTerm ((TmTensor tm1 tm2, TyTensor ty1 ty2):cxt) (tm, ty) =
  checkTerm ((tm1,ty1):(tm2,ty2):cxt) (tm, ty)
-- arrow_l
checkTerm ((TmArrow tm1 tm2, TyFunc ty1 ty2):cxt) (tm, ty) = do
  cxt' <- checkTerm cxt (tm1, ty1)
  checkTerm ((tm2, ty2):cxt) (tm, ty)
-- fold_l
checkTerm ((TmFold um, TyRec x uy):cxt) (tm, ty) =
  checkTerm ((um, subst uy x (TyRec x uy)):cxt) (tm, ty)
-- lin_l
checkTerm ((TmLin tm1 tm2, uy):cxt) (tm, ty) = do
  cxt1 <- checkTerm ((tm1,uy):cxt) (tm, ty)
  cxt2 <- checkTerm ((tm2,uy):cxt) (tm, ty)
  if cxt1 == cxt2 --🤔
    then pure cxt1
    else typeCheckForTermError "remainder contexts between lin are different."
-- label_l
checkTerm ((TmLabel (Ident s) um, uy):cxt) (tm, ty) = do
  labels <- ask
  case Map.lookup s labels of
    Just (Label sy) -> checkTerm ((um, sy):cxt) (tm, ty)
    Nothing -> labelNotFoundError s
-- unit_r
checkTerm [] (TmUnit, TyUnit) =
  pure []
-- inl_r
checkTerm cxt (TmLeft tm1, TySum ty1 ty2) =
  checkTerm cxt (tm1, ty1)
-- inr_r
checkTerm cxt (TmRight tm2, TySum ty1 ty2) =
  checkTerm cxt (tm2, ty2)
-- tensor_r
checkTerm cxt (TmTensor tm1 tm2, TyTensor ty1 ty2) = do
  cxt' <- checkTerm cxt (tm1, ty1)
  checkTerm cxt' (tm2, ty2)
-- arrow_r
checkTerm cxt (TmArrow tm1 tm2, TyFunc ty1 ty2) =
  checkTerm ((tm1, ty1):cxt) (tm2, ty2)
-- fold_r
checkTerm cxt (TmFold tm, TyRec x ty) =
  checkTerm cxt (tm, subst ty x (TyRec x ty))
-- lin_r
checkTerm cxt (TmLin tm1 tm2, ty) = do
  cxt1 <- checkTerm cxt (tm1, ty)
  cxt2 <- checkTerm cxt (tm2, ty)
  if cxt1 == cxt2 --🤔
    then pure cxt1
    else typeCheckForTermError "remainder contexts between lin are different."
-- label_r
checkTerm cxt (TmLabel (Ident s) tm, ty) = do
  labels <- ask
  case Map.lookup s labels of
    Just (Label uy) -> checkTerm cxt (tm, uy)
    Nothing -> labelNotFoundError s
-- otherwise
checkTerm cxt (tm, ty) = typeCheckForTermError $ show cxt ++ "|-" ++ show tm ++ ":" ++ show ty

-- Termの中の自由変数のリスト
fv :: Term -> Check [Ident]
fv (TmVar x) = pure [x]
fv TmUnit = pure []
fv (TmLeft tm1) = fv tm1
fv (TmRight tm2) = fv tm2
fv (TmTensor tm1 tm2) = do
  fv1 <- fv tm1
  fv2 <- fv tm2
  pure $ fv1 ++ fv2
fv (TmArrow tm1 tm2) = do
  fv1 <- fv tm2
  fv2 <- fv tm1
  pure $ fv1 \\ fv2 -- 当然束縛変数は除く
fv (TmFold tm) = fv tm
fv (TmLin tm1 tm2) = do -- Linは左右で同じ自由変数を持つこと
  fv1 <- fv tm1
  fv2 <- fv tm2
  if fv1 == fv2
    then pure fv1
    else throwError ""
fv (TmLabel _ tm) = fv tm

checkExpr :: Expr -> Type -> Check String
checkExpr (ExTerm tm) ty = do
  cxt <- checkTerm [] (tm, ty)
  if null cxt
    then pure ""
    else typeCheckForTermError "Context must be empty after type check."
checkExpr (ExApp ex tm) ty =
  throwError $ "checkExpr: App is not Implemented yet."
checkExpr (ExComp ex1 ex2) (TyFunc t1 t3) =
  throwError $ "checkExpr: Comp is not implemented yet."
checkExpr (ExFlip ex) (TyFunc t1 t2) =
  checkExpr ex (TyFunc t2 t1)
checkExpr (ExTrace tm (Ident s) ty1) ty2 =
  local (Map.insert s (Label ty1)) (checkExpr (ExTerm tm) ty2)