{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Author: MountainSeal
module Omnirev.EvalOmnirev(check) where


import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.List as List
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


data Sign = Pos | Neg deriving (Eq, Ord, Show, Read)
-- 符号をできるだけ(unitか変数直下まで)下げる
downOpp :: Term -> Term
downOpp tm = force Pos tm
  where force Pos (TmVar x)          = TmVar x
        force Neg (TmVar x)          = TmOpp $ TmVar x
        force Pos TmUnit             = TmUnit
        force Neg TmUnit             = TmOpp TmUnit
        force sig (TmLeft  tm)       = TmLeft  $ force sig tm
        force sig (TmRight tm)       = TmRight $ force sig tm
        force sig (TmTensor tm1 tm2) = TmTensor (force sig tm1) (force sig tm2)
        force sig (TmArrow  tm1 tm2) = TmArrow  (force sig tm1) (force sig tm2)
        force sig (TmFold ty tm)     = TmFold ty $ force sig tm
        force sig (TmTrace tm ty)    = TmTrace (force sig tm) ty
        force sig (TmLin tm1 tm2)    = TmLin (force sig tm1) (force sig tm2)
        force Pos (TmOpp tm)         = force Neg tm
        force Neg (TmOpp tm)         = force Pos tm

-- 線形結合をできるだけ上げる
upLin :: Term -> Term
upLin (TmVar x) =
  TmVar x
upLin (TmUnit)  =
  TmUnit
upLin (TmLeft tm) =
  case upLin tm of
    TmLin tm1 tm2 -> TmLin (TmLeft tm1) (TmLeft tm2)
    tm'           -> TmLeft tm'
upLin (TmRight tm) =
  case upLin tm of
    TmLin tm1 tm2 -> TmLin (TmRight tm1) (TmRight tm2)
    tm'           -> TmRight tm'
upLin (TmTensor tm1 tm2) =
  case (upLin tm1, upLin tm2) of
    (TmLin tm11 tm12, TmLin tm21 tm22) -> TmLin (TmLin (TmTensor tm11 tm21) (TmTensor tm11 tm22)) (TmLin (TmTensor tm12 tm21) (TmTensor tm12 tm22))
    (TmLin tm11 tm12, tm2')            -> TmLin (TmTensor tm11 tm2') (TmTensor tm12 tm2')
    (tm1', TmLin tm21 tm22)            -> TmLin (TmTensor tm1' tm21) (TmTensor tm1' tm22)
    (tm1', tm2')                       -> TmTensor tm1' tm2'
upLin (TmArrow tm1 tm2) =
  case (upLin tm1, upLin tm2) of
    (TmLin tm11 tm12, TmLin tm21 tm22) -> TmLin (TmLin (TmArrow tm11 tm21) (TmArrow tm11 tm22)) (TmLin (TmArrow tm12 tm21) (TmArrow tm12 tm22))
    (TmLin tm11 tm12, tm2')            -> TmLin (TmArrow tm11 tm2') (TmArrow tm12 tm2')
    (tm1', TmLin tm21 tm22)            -> TmLin (TmArrow tm1' tm21) (TmArrow tm1' tm22)
    (tm1', tm2')                       -> TmArrow tm1' tm2'
upLin (TmFold ty tm) =
  case upLin tm of
    TmLin tm1 tm2 -> TmLin (TmFold ty tm1) (TmFold ty tm2)
    tm'           -> TmFold ty tm'
upLin (TmTrace tm ty) =
  case (upLin tm) of
    TmLin tm1 tm2 -> TmLin (TmTrace tm1 ty) (TmTrace tm2 ty)
    tm'           -> TmTrace tm' ty
upLin (TmLin tm1 tm2) =
  TmLin (upLin tm1) (upLin tm2)
upLin (TmOpp tm) =
  case (upLin tm) of
    TmLin tm1 tm2 -> TmLin (TmOpp tm1) (TmOpp tm2)
    tm'           -> TmOpp tm'


-- +に関して再帰的に半順序順にソートする
-- ソートの実態
mysort :: [(Int, Term)] -> [(Int, Term)]
mysort = sortBy compare
-- mysort = sortBy (\(i1,t1) (i2,t2) -> (i2 `compare` i1) <> (t1 `compare` t2))

-- +に関してリストで列挙する(Intは「+」だけを一番上から数えたときの深さ)
-- upLinで「+」が全て上に揃っている前提
enum :: Int -> Term -> [(Int, Term)]
enum n (TmLin tm1 tm2)    = tms1 ++ tms2
  where tms1 = mysort $ enum (n+1) tm1
        tms2 = mysort $ enum (n+1) tm2
enum n tm = [(n, tm)]

-- enumでLinがバラされた前提
-- Nothingが融合・相殺不能
-- TmNullが相殺
fusion :: Term -> Term -> Maybe Term
fusion (TmVar x) (TmVar y)
  | x == y = Just $ TmVar x
fusion (TmVar x) (TmOpp (TmVar y))
  | x == y = Just TmNull
fusion (TmOpp (TmVar x)) (TmVar y)
  | x == y = Just TmNull
fusion (TmOpp (TmVar x)) (TmOpp (TmVar y))
  | x == y = Just $ TmOpp $ TmVar x
fusion TmUnit TmUnit =
  Just TmUnit
fusion TmUnit (TmOpp TmUnit) =
  Just TmNull
fusion (TmOpp TmUnit) TmUnit =
  Just TmNull
fusion (TmOpp TmUnit) (TmOpp TmUnit) =
  Just $ TmOpp TmUnit
fusion (TmLeft tm1) (TmLeft tm2) =
  case (fusion tm1 tm2) of
    Nothing -> Nothing
    Just TmNull -> Just TmNull
    Just tm -> Just $ TmLeft tm
fusion (TmRight tm1) (TmRight tm2) =
  case (fusion tm1 tm2) of
    Nothing -> Nothing
    Just TmNull -> Just TmNull
    Just tm -> Just $ TmRight tm
fusion (TmTensor tm11 tm12) (TmTensor tm21 tm22) =
  case (fusion tm11 tm21, fusion tm12 tm22) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just TmNull, Just TmNull) -> Just TmNull
    (Just TmNull, Just _)      -> Nothing
    (Just _, Just TmNull)      -> Nothing
    (Just tm1, Just tm2)       -> Just $ TmTensor tm1 tm2
fusion (TmArrow tm11 tm12) (TmArrow tm21 tm22) =
  case (fusion tm11 tm21, fusion tm12 tm22) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just TmNull, Just TmNull) -> Just TmNull
    (Just TmNull, Just _)      -> Nothing
    (Just _, Just TmNull)      -> Nothing
    (Just tm1, Just tm2)       -> Just $ TmArrow tm1 tm2
fusion (TmFold ty1 tm1) (TmFold ty2 tm2)
  | ty1 `alphaEquiv` ty2 =
    case (fusion tm1 tm2) of
      Nothing -> Nothing
      Just TmNull -> Just TmNull
      Just tm -> Just $ TmFold ty1 tm
  | otherwise = Nothing
fusion (TmTrace tm1 ty1) (TmTrace tm2 ty2)
  | ty1 `alphaEquiv` ty2 =
     case (fusion tm1 tm2) of
       Nothing -> Nothing
       Just TmNull -> Just TmNull
       Just tm -> Just $ TmTrace tm ty1
fusion _ _ = Nothing

merge ::  [(Int, Term)] -> Term
merge ts = case fld ts of
  [(_, tm)] -> tm
  where fld :: [(Int, Term)] -> [(Int, Term)]
        fld [(n, t)] = [(n, t)]
        fld ((n1,t1) : (n2,t2) : ts) = if n1 == n2
          then
            case fusion t1 t2 of
              Just tm' -> fld $ (n1-1, tm'):ts
              Nothing  -> fld $ (n1-1, TmLin t1 t2):ts
          else fld $ (n1,t1) : (fld $ (n2,t2):ts)

repr :: Term -> Term
repr = merge . enum 0 . upLin . downOpp