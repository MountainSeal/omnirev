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

newtype Label = Label String
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

unknownVarError :: Show a => a -> Check b
unknownVarError x = throwError $ "variable not found: " ++ show x

checkProg :: Program -> Check String
checkProg (Prog []) = throwError "There is no program"
checkProg (Prog defs) = checkDefs defs

checkDefs :: [Def] -> Check String
checkDefs []   = pure ""
checkDefs [d]  = checkDef d
checkDefs (d:ds) = (++) <$> checkDef d <*> checkDefs ds

checkDef :: Def -> Check String
checkDef d = throwError $ "under construction: checkDef"

-- check type formation (see Type Formation rules)
checkType :: [Ident] -> Type -> Check String
checkType cxt (TyVar (Ident s)) = if Ident s `elem` cxt
  then pure ""
  else do -- Aliasのほうの変数だと解釈する
    env <- get
    case Map.lookup s env of
      Just AType{} -> pure "" -- Aliasは検査済なので何もしない
      Just AExpr{} -> throwError $ "has already defined as Expression: " ++ s
      Nothing      -> unknownVarError s
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
checkTerm :: Context -> Term -> Check String
checkTerm cxt t = throwError $ "under construction: checkTerm"

checkExpr :: Expr -> Check String
checkExpr e = throwError $ "under construction: checkExpr"