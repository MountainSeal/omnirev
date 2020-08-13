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
  | ATerm Term Type
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

dupDefTermError :: Show a => a -> Check b
dupDefTermError x = throwError $ "The alias is already defined as Term:" ++ show x

dupDefExprError :: Show a => a -> Check b
dupDefExprError x = throwError $ "The alias is already defined as Expression: " ++ show x

dupDefError :: Show a => Alias -> a -> Check b
dupDefError AType{} = dupDefTypeError
dupDefError ATerm{} = dupDefTermError
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
      ty' <- purify [] ty
      checkType [] ty'
      modify $ Map.insert s (AType ty')
      pure ""
    Just v -> dupDefError v s
checkDef (DTerm (Ident s) ty tm) = do
  env <- get
  case Map.lookup s env of
    Nothing -> do
      ty' <- purify [] ty
      checkType [] ty'
      tm' <- tmPurify tm
      checkTerm [] (tm', ty')
      modify $ Map.insert s (ATerm tm' ty')
      pure ""
    Just v -> dupDefError v s
checkDef (DExpr (Ident s) ty ex) = do
  env <- get
  case Map.lookup s env of
    Nothing -> do
      ty' <- purify [] ty
      checkType [] ty'
      ex' <- exPurify ex
      checkExpr ex' ty'
      modify $ Map.insert s (AExpr ex' ty')
      pure ""
    Just v -> dupDefError v s

-- The `purify` function replace type variables.
purify :: [Ident] -> Type -> Check Type
purify cxt (TyVar (Ident s)) =
  if Ident s `elem` cxt
    then pure (TyVar (Ident s))
    else do
      env <- get
      case Map.lookup s env of
        Just (AType ty) -> pure ty
        Just AExpr{}    -> dupDefExprError s
        Just ATerm{}    -> dupDefTermError s
        Nothing         -> defNotFoundError s
purify cxt TyUnit =
  pure TyUnit
purify cxt (TySum ty1 ty2) = do
  ty1' <- purify cxt ty1
  ty2' <- purify cxt ty2
  pure $ TySum ty1' ty2'
purify cxt (TyTensor ty1 ty2) = do
  ty1' <- purify cxt ty1
  ty2' <- purify cxt ty2
  pure $ TyTensor ty1' ty2'
purify cxt (TyFunc ty1 ty2) = do
  ty1' <- purify cxt ty1
  ty2' <- purify cxt ty2
  pure $ TyFunc ty1' ty2'
purify cxt (TyRec x ty) = do
  ty' <- purify (x:cxt) ty
  pure $ TyRec x ty'

-- termのaliasを置き換え，typeのaliasを書き換える関数
tmPurify :: Term -> Check Term
tmPurify (TmVar (Ident s)) = do
    env <- get
    case Map.lookup s env of
      Just AType{}      -> dupDefTypeError s
      Just (ATerm tm _) -> pure tm
      Just AExpr{}      -> dupDefExprError s
      Nothing           -> pure $ TmVar $ Ident s
tmPurify TmUnit = pure TmUnit
tmPurify (TmLeft tm) = do
  tm' <- tmPurify tm
  pure $ TmLeft tm'
tmPurify (TmRight tm) = do
  tm' <- tmPurify tm
  pure $ TmRight tm'
tmPurify (TmTensor tm1 tm2) = do
  tm1' <- tmPurify tm1
  tm2' <- tmPurify tm2
  pure $ TmTensor tm1' tm2'
tmPurify (TmArrow tm1 tm2) = do
  tm1' <- tmPurify tm1
  tm2' <- tmPurify tm2
  pure $ TmArrow tm1' tm2'
tmPurify (TmFold ty tm) = do
  ty' <- purify [] ty -- typeの置き換え
  tm' <- tmPurify tm
  pure $ TmFold ty' tm'
tmPurify (TmLin tm1 tm2) = do
  tm1' <- tmPurify tm1
  tm2' <- tmPurify tm2
  pure $ TmLin tm1' tm2'
tmPurify (TmOpp tm) = do
  tm' <- tmPurify tm
  pure $ TmOpp tm'
tmPurify (TmTrace tm ty) = do
  tm' <- tmPurify tm
  ty' <- purify [] ty -- typeの置き換え
  pure $ TmTrace tm' ty'

-- exprのtmPurify
exPurify :: Expr -> Check Expr
exPurify (ExTerm (TmVar (Ident s))) = do
  env <- get
  case Map.lookup s env of
    Just AType{}       -> dupDefTypeError s
    Just ATerm{}       -> dupDefTermError s
    Just (AExpr ex _)  -> pure ex
    Nothing            -> defNotFoundError s
exPurify (ExTerm tm) = do
  tm' <- tmPurify tm
  pure $ ExTerm tm'
exPurify (ExApp ex1 ex2) = do
  ex1' <- exPurify ex1
  ex2' <- exPurify ex2
  pure $ ExApp ex1' ex2'
exPurify (ExFlip ex) = do
  ex' <- exPurify ex
  pure $ ExFlip ex'

-- check type formation (see Type Formation rules)
checkType :: [Ident] -> Type -> Check String
checkType cxt (TyVar (Ident s))
  | Ident s `elem` cxt = pure ""
  | otherwise          = throwError $ "type variable not found." ++ show s
checkType cxt TyUnit =
  pure ""
checkType cxt (TySum t1 t2) =
  checkType cxt t1 >> checkType cxt t2
checkType cxt (TyTensor t1 t2) =
  checkType cxt t1 >> checkType cxt t2
checkType cxt (TyFunc t1 t2) =
  checkType cxt t1 >> checkType cxt t2
checkType cxt (TyRec x t)
  | x `elem` cxt = throwError "type variable already bounded."
  | otherwise    = checkType (x:cxt) t

subst :: Type -> Ident -> Type -> Type
subst (TyVar y)        x s = if y == x then s else TyVar y
subst TyUnit           x s = TyUnit
subst (TySum t1 t2)    x s = TySum (subst t1 x s) (subst t2 x s)
subst (TyTensor t1 t2) x s = TyTensor (subst t1 x s) (subst t2 x s)
subst (TyFunc t1 t2)   x s = TyFunc (subst t1 x s) (subst t2 x s)
subst (TyRec y t)      x s = TyRec y (subst t x s)

(~>) :: Ident -> Type -> (Type -> Type)
x ~> ty = \t -> subst t x ty

--型変数（自由束縛にかかわらず全て）
tyvars :: Type -> [Ident]
tyvars (TyVar x) = [x]
tyvars  TyUnit   = []
tyvars (TySum    t1 t2) = tyvars t1 ++ tyvars t2
tyvars (TyTensor t1 t2) = tyvars t1 ++ tyvars t2
tyvars (TyFunc   t1 t2) = tyvars t1 ++ tyvars t2
tyvars (TyRec x t) = let v = tyvars t in if x `elem` v then v else x:v

-- 束縛変数が異なっていても同値であることを判定
alphaEquiv :: Type -> Type -> Bool
alphaEquiv (TyVar x) (TyVar y)
  | x == y     = True
  | otherwise  = False
alphaEquiv  TyUnit           TyUnit          = True
alphaEquiv (TySum    s1 s2) (TySum    t1 t2) = alphaEquiv s1 t1 && alphaEquiv s2 t2
alphaEquiv (TyTensor s1 s2) (TyTensor t1 t2) = alphaEquiv s1 t1 && alphaEquiv s2 t2
alphaEquiv (TyFunc   s1 s2) (TyFunc   t1 t2) = alphaEquiv s1 t1 && alphaEquiv s2 t2
alphaEquiv (TyRec x s) (TyRec y t)
  | x == y    = alphaEquiv s t
  | otherwise = alphaEquiv s' t'
    where
      svs = tyvars (TyRec x s)
      tvs = tyvars (TyRec y t)
      f (Ident s) = s
      uv = TyVar $ Ident $ unwords $ (map f svs) ++ (map f tvs)
      s' = (x ~> uv)s
      t' = (y ~> uv)t

-- Termが変数の場合は後ろに追加，そうでない場合は前に追加
-- なんでこんなことをするかというと，コンテキスト中の変数を含まない項を必ず処理させるため
-- 本来の型検査であればコンテキストの順序に依存しないが，型検査のアルゴリズム実装にあたってはコンテキストの順序は代表元だけで処理したいのじゃ
ext :: (Term, Type) -> Context -> Context
ext (TmVar i, ty) cxt = cxt ++ [(TmVar i, ty)]
ext (tm     , ty) cxt = (tm,ty):cxt

type Context = [(Term, Type)]
checkTerm :: Context -> (Term, Type) -> Check Context
-- variable
checkTerm cxt (TmVar i, ty) =
  case lookup (TmVar i) cxt of
    Just ty' -> if ty == ty'
      then pure $ delete (TmVar i, ty') cxt
      else typeCheckForTermError "same variable but different type."
    Nothing -> typeCheckForTermError $ "not found variable." ++ show i
-- unit_l
checkTerm ((TmUnit, TyUnit):cxt) (tm, ty) =
  checkTerm cxt (tm, ty)
-- inl_l
checkTerm ((TmLeft tm1, TySum ty1 ty2):cxt) (tm, ty) = do
  let cxt' = (tm1, ty1) `ext` cxt
  checkTerm cxt' (tm, ty)
  -- inr_l
checkTerm ((TmRight tm2, TySum ty1 ty2):cxt) (tm, ty) = do
  let cxt' = (tm2, ty2) `ext` cxt
  checkTerm cxt' (tm, ty)
-- tensor_l
checkTerm ((TmTensor tm1 tm2, TyTensor ty1 ty2):cxt) (tm, ty) = do
  let cxt' = (tm2, ty2) `ext` ((tm1, ty1) `ext` cxt)
  checkTerm cxt' (tm, ty)
-- arrow_l
checkTerm ((TmArrow tm1 tm2, TyFunc ty1 ty2):cxt) (tm, ty) = do
  cxt' <- checkTerm cxt (tm1, ty1)
  let cxt'' = (tm2, ty2) `ext` cxt'
  checkTerm cxt'' (tm, ty)
-- fold_l
checkTerm ((TmFold sy um, TyRec x uy):cxt) (tm, ty)
  | alphaEquiv sy (TyRec x uy) = do
    let uy'  = (x ~> TyRec x uy) uy
        cxt' = (um, uy') `ext` cxt
    checkTerm cxt' (tm, ty)
  | otherwise                  = throwError "not equivalent"
-- lin_l
checkTerm ((TmLin tm1 tm2, uy):cxt) (tm, ty) = do
  cxt1 <- checkTerm (ext (tm1,uy) cxt) (tm, ty)
  cxt2 <- checkTerm (ext (tm2,uy) cxt) (tm, ty)
  if cxt1 == cxt2 --🤔
    then pure cxt1
    else typeCheckForTermError "[L]remainder contexts between lin are different."
-- opp_l
checkTerm ((TmOpp um, uy):cxt) (tm, ty) = do
  let cxt' = (um, uy) `ext` cxt
  checkTerm cxt' (tm, ty)
-- trace_l
checkTerm ((TmTrace um sy, TyFunc uy1 uy2):cxt) (tm, ty) = do
  let cxt' = (um, TyFunc (TySum sy uy1) (TySum sy uy2)) `ext` cxt
  checkTerm cxt' (tm, ty)

-- unit_r
checkTerm cxt (TmUnit, TyUnit) =
  pure cxt
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
  checkTerm (ext (tm1, ty1) cxt) (tm2, ty2)
-- fold_r
checkTerm cxt (TmFold uy tm, TyRec x ty)
  | alphaEquiv uy (TyRec x ty) = do
    let ty' = (x ~> TyRec x ty) ty
    checkTerm cxt (tm, ty')
  | otherwise                  = throwError "not equivalent"
-- lin_r
checkTerm cxt (TmLin tm1 tm2, ty) = do
  cxt1 <- checkTerm cxt (tm1, ty)
  cxt2 <- checkTerm cxt (tm2, ty)
  if cxt1 == cxt2 --🤔
    then pure cxt1
    else typeCheckForTermError $ "[R]remainder contexts between lin are different."
-- opp_r
checkTerm cxt (TmOpp tm, ty) =
  checkTerm cxt (tm, ty)
-- trace_r
checkTerm cxt (TmTrace tm uy, TyFunc ty1 ty2) =
  checkTerm cxt (tm, TyFunc (TySum uy ty1) (TySum uy ty2))

-- otherwise
checkTerm cxt (tm, ty) = typeCheckForTermError $ show cxt ++ "|-" ++ show tm ++ ":" ++ show ty

-- Termの中の自由変数のリスト
tmFV :: Term -> Check [Ident]
tmFV (TmVar x) = pure [x]
tmFV TmUnit = pure []
tmFV (TmLeft tm1) = tmFV tm1
tmFV (TmRight tm2) = tmFV tm2
tmFV (TmTensor tm1 tm2) = do
  tmFV1 <- tmFV tm1
  tmFV2 <- tmFV tm2
  pure $ tmFV1 ++ tmFV2
tmFV (TmArrow tm1 tm2) = do
  tmFV1 <- tmFV tm2
  tmFV2 <- tmFV tm1
  pure $ tmFV1 \\ tmFV2 -- 当然束縛変数は除く
tmFV (TmFold _ tm) = tmFV tm
tmFV (TmTrace tm _) = tmFV tm
tmFV (TmLin tm1 tm2) = do -- Linは左右で同じ自由変数を持つこと
  tmFV1 <- tmFV tm1
  tmFV2 <- tmFV tm2
  if tmFV1 == tmFV2
    then pure tmFV1
    else throwError ""
tmFV (TmOpp tm) = tmFV tm

-- Typeの中の自由変数のリスト
tyFV :: Type -> [Ident]
tyFV (TyVar x)          = [x]
tyFV  TyUnit            = []
tyFV (TySum    ty1 ty2) = tyFV ty1 ++ tyFV ty2
tyFV (TyTensor ty1 ty2) = tyFV ty1 ++ tyFV ty2
tyFV (TyFunc   ty1 ty2) = tyFV ty1 ++ tyFV ty2
tyFV (TyRec x ty)       = delete x $ tyFV ty

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
checkExpr (ExTrace tm (Ident s) ty1) ty2 = do
  ty1' <- purify [] ty1 -- purifyを消しては行けない（戒め）
  checkType [] ty1' -- この型検査してなかった・・・
  local (Map.insert s (Label ty1')) (checkExpr (ExTerm tm) ty2)

-- TmFoldのTypeからAliasを書き換えるためだけの関数
tmPurify :: Term -> Check Term
tmPurify (TmVar x) = pure $ TmVar x
tmPurify TmUnit = pure TmUnit
tmPurify (TmLeft tm) = do
  tm' <- tmPurify tm
  pure $ TmLeft tm'
tmPurify (TmRight tm) = do
  tm' <- tmPurify tm
  pure $ TmRight tm'
tmPurify (TmTensor tm1 tm2) = do
  tm1' <- tmPurify tm1
  tm2' <- tmPurify tm2
  pure $ TmTensor tm1' tm2'
tmPurify (TmArrow tm1 tm2) = do
  tm1' <- tmPurify tm1
  tm2' <- tmPurify tm2
  pure $ TmArrow tm1' tm2'
tmPurify (TmFold ty tm) = do
  ty' <- purify [] ty -- やりたいことは実際ココ
  tm' <- tmPurify tm
  pure $ TmFold ty' tm'
tmPurify (TmLin tm1 tm2) = do
  tm1' <- tmPurify tm1
  tm2' <- tmPurify tm2
  pure $ TmLin tm1' tm2'
tmPurify (TmLabel l tm) = do
  tm' <- tmPurify tm
  pure $ TmLabel l tm'