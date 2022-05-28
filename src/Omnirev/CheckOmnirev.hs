{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Author: MountainSeal
module Omnirev.CheckOmnirev(check, Typed, (~>), checkComposition, tyEquiv, tyvars, tmvars) where


import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Bifunctor
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Omnirev.AbsOmnirev
import Omnirev.PrintOmnirev

import Omnirev.ErrM as ErrM
import Omnirev.Log
import Omnirev.Env
import Data.Maybe

-- https://qiita.com/HirotoShioi/items/8a6107434337b30ce457 を参考
newtype Check a = Check ((StateT (Env Alias) (ExceptT String (WriterT Logs Identity))) a)
  deriving (Functor, Applicative, Monad, MonadWriter Logs, MonadState (Env Alias), MonadError String)

-- TO DO LIST
-- Mapにすると順序が勝手にソートされてEvalのときに評価順が変わるので、評価順をリストとして渡すか順序が維持されるように変更する
-- テストの内容を再作成する（ユニットテスト）
-- monad-loggerを利用したロギングに変更
-- optparse-applicativeを利用してコマンドのオプション処理をする

-- Either String a （Stringが例外）
runCheck :: Check a -> Env Alias -> (Either String a, Logs)
runCheck (Check m) env = runIdentity (runWriterT (runExceptT (evalStateT m env)))

-- Entrypoint of type checking
check :: Program -> Err (([Ident], Env Alias), Logs)
check p = case runCheck (checkProg p) M.empty of
  (Left  str, logs) -> Bad $ unlines logs
  (Right res, logs) -> Ok (res, logs)

-- 関数の合成に関して，中間の型が唯一に定まるか確認する
checkComposition :: Term -> Type -> Err Type
checkComposition (TmComp tm1 tm2) (TyFunc ty1 ty2) =
  case runCheck (checkComp (TmComp tm1 tm2) (TyFunc ty1 ty2)) M.empty of
    (Left  str, logs) -> Bad $ unlines logs
    (Right typ, logs) -> Ok typ
checkComposition _ _ = Bad $ unlines []

defNotFoundError :: (Show a, Print a) => a -> Check b
defNotFoundError x = do
  tell [infoLog $ "The alias not found: " ++ printTree x]
  throwError $ "The alias not found: " ++ printTree x

dupDefTypeError :: (Show a, Print a) => a -> Check b
dupDefTypeError x = do
  tell [infoLog $ "The alias is already defined as Type: " ++ printTree x]
  throwError $ "The alias is already defined as Type: " ++ printTree x

dupDefTermError :: (Show a, Print a) => a -> Check b
dupDefTermError x = do
  tell [infoLog $ "The alias is already defined as Term:" ++ printTree x]
  throwError $ "The alias is already defined as Term:" ++ printTree x

dupDefExprError :: (Show a, Print a) => a -> Check b
dupDefExprError x = do
  tell [infoLog $ "The alias is already defined as Expression: " ++ printTree x]
  throwError $ "The alias is already defined as Expression: " ++ printTree x

dupDefError :: (Show a, Print a) => Alias -> a -> Check b
dupDefError AType{} = dupDefTypeError
dupDefError ATerm{} = dupDefTermError
dupDefError AExpr{} = dupDefExprError
dupDefError AVar    = \x -> do
  tell [infoLog $ "Bounded variable: " ++ printTree x]
  throwError $ "Bounded variable: " ++ printTree x

typeVarNotFoundError :: (Show a, Print a) => a -> Check b
typeVarNotFoundError x = do
  tell [infoLog $ "type variable not found: " ++ printTree x]
  throwError $ "type variable not found: " ++ printTree x

typeVarAlreadyBoundedError :: (Show a, Print a) => a -> Check b
typeVarAlreadyBoundedError x = do
  tell [infoLog $ "type variable already bounded: " ++ printTree x]
  throwError $ "type variable already bounded: " ++ printTree x

inferenceError :: (Show a, Print a) => LogLevel -> a -> Check b
inferenceError ll x = do
  tell [logAt ll $ "type inference failed: " ++ printTree x]
  throwError $ "type inference failed: " ++ printTree x

unificationFailError :: Check a
unificationFailError = do
  tell [infoLog "unification fail"]
  throwError "unification fail"

unificationMismatchError :: (Show a, Print a) => a -> a -> Check b
unificationMismatchError x y = do
  let str = "unification success, but mismatch: " ++ printTree x ++ " <> " ++ printTree y
  tell [debugLog str]
  throwError str

checkProg :: Program -> Check ([Ident], Env Alias)
checkProg (Prog []) = do
  tell [infoLog "there is no program"]
  throwError "there is no program"
checkProg (Prog defs) = do
  tell [infoLog "check start"]
  ids <- checkDefs defs
  tell [infoLog "check finish"]
  env <- get
  pure (f env ids, env)
  where
    -- ExprのIdentのみのリストを返す
    f :: Env Alias -> [Ident] -> [Ident]
    f env ls = filter (g env) ls
    g :: Env Alias -> Ident -> Bool
    g env (Ident s) = case M.lookup s env of
      Nothing -> False
      Just AType{} -> False
      Just ATerm{} -> False
      Just AExpr{} -> True
      Just AVar{}  -> False

checkDefs :: [Def] -> Check [Ident]
checkDefs [] = pure []
checkDefs (d:ds) = do
  i <- checkDef d
  is <- checkDefs ds
  pure $ i:is
-- checkDefs = foldr (\ d -> (<*>) ((++) <$> checkDef d)) (pure [])

checkDef :: Def -> Check Ident
checkDef (DType (Ident s) ty) = do
  tell [infoLog $ "check type of alias " ++ printTree s]
  env <- get
  case M.lookup s env of
    Nothing -> do
      tell [debugLog "purify " ++ printTree ty]
      ty' <- purify [] ty
      tell [debugLog "checkType " ++ printTree ty']
      checkType [] ty'
      modify $ M.insert s (AType ty')
      pure $ Ident s
    Just v -> dupDefError v s
checkDef (DTerm (Ident s) ty tm) = do
  tell [infoLog $ "check term of alias " ++ printTree s]
  env <- get
  case M.lookup s env of
    Nothing -> do
      tell [debugLog "purify " ++ printTree ty]
      ty' <- purify [] ty
      tell [debugLog "checkType " ++ printTree ty']
      checkType [] ty'
      tell [debugLog "tmPurify " ++ printTree tm]
      tm' <- tmPurify tm
      tell [debugLog "infer " ++ printTree tm']
      x <- TyVar <$> uvar
      (cxt, cs) <- infer [] (tm', x)
      cvars
      tell [debugLog "unify " ++ constraints cs]
      if null cxt
        then do
          sb <- unify $ cs ++ [(x,ty')]
          if sb x `tyEquiv` ty'
            then tell [debugLog "unification success: " ++ printTree (sb x)]
            else
              unificationMismatchError ty' (sb x)
        else inferenceError Info "context must be empty after type inference."
      modify $ M.insert s (ATerm tm' ty')
      pure $ Ident s
    Just v -> dupDefError v s
checkDef (DExpr (Ident s) ty ex) = do
  tell [infoLog $ "check expr of alias " ++ printTree s]
  env <- get
  case M.lookup s env of
    Nothing -> do
      tell [debugLog "purify " ++ printTree ty]
      ty' <- purify [] ty
      tell [debugLog "checkType " ++ printTree ty']
      checkType [] ty'
      tell [debugLog "exPurify " ++ printTree ex]
      ex' <- exPurify ex
      tell [debugLog "infer " ++ printTree ex']
      x <- TyVar <$> uvar
      cs <- inferExpr (ex', x)
      cvars
      tell [debugLog "unify " ++ constraints cs]
      sb <- unify cs
      if sb x `tyEquiv` ty'
        then tell [debugLog "unification success: " ++ printTree (sb x)]
        else unificationMismatchError ty' (sb x)
      modify $ M.insert s (AExpr ex' ty')
      pure $ Ident s
    Just v -> dupDefError v s

-- 関数合成値と関数型を受け取って，中間の型について入力側と出力側それぞれで型推論可能か判定し，可能なら中間の型を返す
checkComp :: Term -> Type -> Check Type
checkComp (TmComp tm1 tm2) (TyFunc ty1 ty2) = do
  -- めんどくさいから与えられる項と型は検査済みの前提
  x <- TyVar <$> uvar
  (cxt1, cs1) <- infer [] (tm1, TyFunc ty1 x)
  (cxt2, cs2) <- infer [] (tm2, TyFunc x ty2)
  cvars
  sb <- unify $ cs1 ++ cs2
  pure $ sb x
checkComp _ _ = throwError "関数合成の中間の型を得る為の関数だから他の目的で使ったりしたらダメだよ！"

-- The `purify` function replace type variables.
purify :: [Ident] -> Type -> Check Type
purify cxt (TyVar (Ident s)) =
  if Ident s `elem` cxt
    then pure (TyVar (Ident s))
    else do
      env <- get
      case M.lookup s env of
        Just (AType ty) -> pure ty
        Just ATerm{}    -> dupDefTermError s
        Just AExpr{}    -> dupDefExprError s
        Just AVar       -> do
          tell [warnLog $ printTree s ++ "is already defined as binded variable"]
          defNotFoundError s
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
    case M.lookup s env of
      Just AType{}      -> dupDefTypeError s
      Just (ATerm tm _) -> pure tm
      Just AExpr{}      -> dupDefExprError s
      Just AVar         -> do
          tell [warnLog $ printTree s ++ "is already defined as binded variable"]
          defNotFoundError s
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
  ty' <- purify [] ty
  tm' <- tmPurify tm
  pure $ TmFold ty' tm'
tmPurify (TmLin tm1 tm2) = do
  tm1' <- tmPurify tm1
  tm2' <- tmPurify tm2
  pure $ TmLin tm1' tm2'
tmPurify (TmTrace ty tm) = do
  ty' <- purify [] ty
  tm' <- tmPurify tm
  pure $ TmTrace ty' tm'
tmPurify (TmComp tm1 tm2) = do
  tm1' <- tmPurify tm1
  tm2' <- tmPurify tm2
  pure $ TmComp tm1' tm2'
tmPurify (TmFlip tm) = do
  tm' <- tmPurify tm
  pure $ TmFlip tm'
tmPurify TmEmpty = pure TmEmpty
tmPurify TmId = pure TmId

-- exprのtmPurify
exPurify :: Expr -> Check Expr
exPurify (ExTerm (TmVar (Ident s))) = do
  env <- get
  case M.lookup s env of
    Just AType{}       -> dupDefTypeError s
    Just (ATerm tm _)  -> pure $ ExTerm tm
    Just (AExpr ex _)  -> pure ex
    Just AVar          -> do
          tell [warnLog $ printTree s ++ "is already defined as binded variable"]
          defNotFoundError s
    Nothing            -> defNotFoundError s
exPurify (ExTerm tm) = do
  tm' <- tmPurify tm
  pure $ ExTerm tm'
exPurify (ExApp ex tm) = do
  ex' <- exPurify ex
  tm' <- tmPurify tm
  pure $ ExApp ex' tm'

-- check type formation (see Type Formation rules)
checkType :: [Ident] -> Type -> Check String
checkType cxt (TyVar (Ident s))
  | Ident s `elem` cxt = pure ""
  | otherwise          = typeVarNotFoundError s
checkType cxt TyUnit =
  pure ""
checkType cxt (TySum t1 t2) =
  checkType cxt t1 >> checkType cxt t2
checkType cxt (TyTensor t1 t2) =
  checkType cxt t1 >> checkType cxt t2
checkType cxt (TyFunc t1 t2) =
  checkType cxt t1 >> checkType cxt t2
checkType cxt (TyRec x t)
  | x `elem` cxt = typeVarAlreadyBoundedError x
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

subs :: Ident -> Type -> M.Map Ident Type
subs = M.singleton

--型変数（自由束縛にかかわらず全て）
tyvars :: Type -> S.Set Ident
tyvars (TyVar x) = S.singleton x
tyvars  TyUnit   = S.empty
tyvars (TySum    t1 t2) = tyvars t1 `S.union` tyvars t2
tyvars (TyTensor t1 t2) = tyvars t1 `S.union` tyvars t2
tyvars (TyFunc   t1 t2) = tyvars t1 `S.union` tyvars t2
tyvars (TyRec x t) = S.insert x $ tyvars t

-- 束縛変数が異なっていても同値であることを判定
tyEquiv :: Type -> Type -> Bool
tyEquiv (TyVar x) (TyVar y)
  | x == y     = True
  | otherwise  = False
tyEquiv  TyUnit           TyUnit          = True
tyEquiv (TySum    s1 s2) (TySum    t1 t2) = tyEquiv s1 t1 && tyEquiv s2 t2
tyEquiv (TyTensor s1 s2) (TyTensor t1 t2) = tyEquiv s1 t1 && tyEquiv s2 t2
tyEquiv (TyFunc   s1 s2) (TyFunc   t1 t2) = tyEquiv s1 t1 && tyEquiv s2 t2
tyEquiv (TyRec x s) (TyRec y t)
  | x == y    = tyEquiv s t
  | otherwise = tyEquiv s' t'
    where
      svs = tyvars (TyRec x s)
      tvs = tyvars (TyRec y t)
      f (Ident s) = s
      uv = TyVar $ Ident $ unwords $ map f $ S.toList $ S.union svs tvs
      s' = (x ~> uv)s
      t' = (y ~> uv)t
tyEquiv _ _ = False

-- Typeの中の自由変数のリスト
tyFV :: Type -> S.Set Ident
tyFV (TyVar x)          = S.singleton x
tyFV  TyUnit            = S.empty
tyFV (TySum    ty1 ty2) = tyFV ty1 `S.union` tyFV ty2
tyFV (TyTensor ty1 ty2) = tyFV ty1 `S.union` tyFV ty2
tyFV (TyFunc   ty1 ty2) = tyFV ty1 `S.union` tyFV ty2
tyFV (TyRec x ty)       = S.delete x $ tyFV ty

unify :: [(Type, Type)] -> Check (Type -> Type)
unify [] = do
  tell [infoLog $ "constraints: " ++ constraints []]
  pure id
unify ((s,t):ls)
  | s == t = do
    tell [infoLog $ "constraints: " ++ constraints ((s, t):ls)]
    unify ls
unify ((TyVar x, t):ls) = do
  tell [infoLog $ "constraints: " ++ constraints ((TyVar x, t):ls)]
  unif <- unify $ map (bimap sig sig) ls
  pure $ unif . sig
  where
    sig = x ~> t
unify ((s, TyVar x):ls) = do
  tell [infoLog $ "constraints: " ++ constraints ((s, TyVar x):ls)]
  unif <- unify $ map (bimap sig sig) ls
  pure $ unif . sig
  where
    sig = x ~> s
unify ((TyUnit, TyUnit):ls) = do
  tell [infoLog $ "constraints: " ++ constraints ((TyUnit, TyUnit):ls)]
  unify ls
unify ((TySum s1 s2, TySum t1 t2):ls) = do
  tell [infoLog $ "constraints: " ++ constraints ((TySum s1 s2, TySum t1 t2):ls)]
  unify $ (s1,t1) : (s2,t2) : ls
unify ((TyTensor s1 s2, TyTensor t1 t2):ls) = do
  tell [infoLog $ "constraints: " ++ constraints ((TyTensor s1 s2, TyTensor t1 t2):ls)]
  unify $ (s1,t1) : (s2,t2) : ls
unify ((TyFunc s1 s2, TyFunc t1 t2):ls) = do
  tell [infoLog $ "constraints: " ++ constraints ((TyFunc s1 s2, TyFunc t1 t2):ls)]
  unify $ (s1,t1) : (s2,t2) : ls
unify ((TyRec x s, TyRec y t):ls) = do
  tell [infoLog $ "constraints: " ++ constraints ((TyRec x s, TyRec y t):ls)]
  unify $ (TyVar x,TyVar y) : (s,t) : ls
  -- unify $ (s', t') : ls -- ここおかしいからそのうち修正するん
  -- where
  -- -- 新しい変数導入してそれ使ってα変換…
  -- -- x <- TyVar <$> uvar
  --   svs = tyvars (TyRec x s)
  --   tvs = tyvars (TyRec y t)
  --   f (Ident s) = s
  --   uv = TyVar $ Ident $ unwords $ map f $ S.toList $ S.union svs tvs
  --   s' = (x ~> uv) s
  --   t' = (y ~> uv) t
unify cs = do
  tell [infoLog $ "constraints : " ++ constraints cs]
  unificationFailError

-- クソザコフレッシュ変数製造機
-- 未使用の変数名を返す
uvar :: Check Ident
uvar = do
  env <- get
  let u = nextuvar env 0
  modify $ M.insert u AVar -- 環境に使用済みであるという情報を追加している
  tell [infoLog $ "make fresh variable as bind ones: " ++ printTree u]
  pure $ Ident u
  where
    var n = "X_" ++ show n
    nextuvar e n = if M.member (var n) e
      then nextuvar e (n+1)
      else var n

-- remove all X_n
cvars :: Check ()
cvars = do
  modify $ M.filter (AVar /=)
  -- env <- get
  -- tell [debugLog $ "env: " ++ show env]

type Typed = (Term, Type)
type Context = [Typed]

(.:.) :: Term -> Type -> Typed
tm .:. ty = (tm, ty)

-- Termが変数の場合は後ろに追加，そうでない場合は前に追加
-- なんでこんなことをするかというと，コンテキスト中の変数を含まない項を必ず処理させるため
-- 本来の型検査であればコンテキストの順序に依存しないが，型検査のアルゴリズム実装にあたってはコンテキストの順序は代表元だけで処理したいのじゃ
ext :: Typed -> Context -> Context
ext (TmVar i, ty) cxt = cxt ++ [(TmVar i, ty)]
ext (tm     , ty) cxt = (tm,ty):cxt

(#) :: Context -> Typed -> Context
cxt # (tm, ty) = ext (tm, ty) cxt

type Constraint = (Type, Type)

typedTerm :: Typed -> String
typedTerm (tm,ty) = intercalate "" [printTree tm, ":", printTree ty]
context :: Context -> String
context cxt = intercalate "#" $ reverse $ map typedTerm cxt
-- Γ#t1:T1 |- t:T |> Γ' | C
judgement :: Context -> Typed -> String
judgement cxt (tm,ty) = unwords [context cxt, "|-", typedTerm (tm,ty)]

constraint :: Constraint -> String
constraint (ty1, ty2) = "(" ++ printTree ty1 ++ "=" ++ printTree ty2 ++ ")"
constraints :: [Constraint] -> String
constraints cs = intercalate "," $ map constraint cs

infer :: Context -> Typed -> Check (Context, [Constraint])
-- variable 
infer cxt (TmVar i, ty) = do
  tell [infoLog $ "variable: " ++ judgement cxt (TmVar i, ty)]
  case lookup (TmVar i) cxt of
    Just ty' -> if ty == ty'
      then do
        let cxt' = delete (TmVar i, ty') cxt
        -- もしまだコンテキストに同じ変数名の項が残っている場合、そちらが適切かもしれない
        if isJust $ lookup (TmVar i) cxt'
          then do
            tell [warnLog $ "the same two or more variables in context: " ++ printTree i]
            tell [warnLog "there is a possibly of a failure even though type inference success in theory.."]
          else
            tell [infoLog $ "the variable appears exactly once in context: " ++ printTree i]
        pure (cxt', [])
      else do
        tell [infoLog "same variable but different type."]
        pure (delete (TmVar i, ty') cxt, [(ty, ty')])
    Nothing -> inferenceError Info $ "not found the variable" ++ printTree i
-- unit_l
infer ((TmUnit, vy) : cxt) (tm, ty) = do
  tell [infoLog $ "unit_l: " ++ judgement ((TmUnit, vy) : cxt) (tm, ty)]
  (cxt', cs) <- infer cxt (tm, ty)
  pure (cxt', (vy, TyUnit) : cs)
-- inl_l
infer ((TmLeft tm1, vy) : cxt) (tm, ty) = do
  tell [infoLog $ "inl_l: " ++ judgement ((TmLeft tm1, vy) : cxt) (tm, ty)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt', cs) <- infer (cxt # (tm1, x1)) (tm, ty)
  pure (cxt', (vy, TySum x1 x2) : cs)
-- inr_l
infer ((TmRight tm2, vy) : cxt) (tm, ty) = do
  tell [infoLog $ "inr_l: " ++ judgement ((TmRight tm2, vy) : cxt) (tm, ty)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt', cs) <- infer (cxt # (tm2, x2)) (tm, ty)
  pure (cxt', (vy, TySum x1 x2) : cs)
-- tensor_l
infer ((TmTensor tm1 tm2, vy) : cxt) (tm, ty) = do
  tell [infoLog $ "tensor_l: " ++ judgement ((TmTensor tm1 tm2, vy) : cxt) (tm, ty)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt', cs) <- infer (cxt # (tm1, x1) # (tm2, x2)) (tm, ty)
  pure (cxt', (vy, TyTensor x1 x2) : cs)
-- arrow_l
infer ((TmArrow tm1 tm2, vy) : cxt) (tm, ty) = do
  tell [infoLog $ "arrow_l: " ++ judgement ((TmArrow tm1 tm2, vy) : cxt) (tm, ty)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt',  cs1) <- infer cxt (tm1, x1)
  (cxt'', cs2) <- infer (cxt # (tm2, x2)) (tm, ty)
  pure (cxt'', (vy, TyFunc x1 x2) : (cs1 ++ cs2))
-- fold_l
infer ((TmFold (TyRec y uy) um, vy) : cxt) (tm, ty) = do
  tell [infoLog $ "fold_l: " ++ judgement ((TmFold (TyRec y uy) um, vy) : cxt) (tm, ty)]
  tell [infoLog $ "checkType " ++ printTree (TyRec y uy)]
  checkType [] (TyRec y uy)
  (cxt', cs) <- infer (cxt # (um, (y ~> TyRec y uy) uy)) (tm, ty)
  pure (cxt', (vy, TyRec y uy): cs)
-- trace_l
infer ((TmTrace uy um, vy) : cxt) (tm, ty) = do
  tell [infoLog $ "trace_l: " ++ judgement ((TmTrace uy um, vy) : cxt) (tm, ty)]
  tell [debugLog "checkType " ++ printTree uy]
  checkType [] uy -- uyに変数の重複がないか確認する必要あり
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt', cs) <- infer (cxt # (um, TyFunc (TySum uy x1) (TySum uy x2))) (tm, ty)
  pure (cxt', (vy, TyFunc x1 x2) : cs)
-- lin_l
infer ((TmLin tm1 tm2, vy) : cxt) (tm, ty) = do
  tell [infoLog $ "lin_l: " ++ judgement ((TmLin tm1 tm2, vy) : cxt) (tm, ty)]
  x <- TyVar <$> uvar
  (cxt1, cs1) <- infer (cxt # (tm1, x)) (tm, ty)
  (cxt2, cs2) <- infer (cxt # (tm2, x)) (tm, ty)
  if cxt1 == cxt2 -- 🤔
    then pure (cxt1, (vy, x) : cs1 ++ cs2)
    else inferenceError Info "remainder contexts between lin are different."
-- comp_l
infer ((TmComp tm1 tm2, vy) : cxt) (tm, ty) = do
  tell [infoLog $ "comp_l: " ++ judgement ((TmComp tm1 tm2, vy) : cxt) (tm, ty)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  x3 <- TyVar <$> uvar
  (cxt', cs) <- infer (cxt # (tm2, TyFunc x2 x3) # (tm1, TyFunc x1 x2)) (tm, ty)
  pure (cxt', (vy, TyFunc x1 x3) : cs)
-- flip_l
infer ((TmFlip um, vy) : cxt) (tm, ty) = do
  tell [infoLog $ "flip_l: " ++ judgement ((TmFlip um, vy) : cxt) (tm, ty)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt', cs) <- infer (cxt # (um, TyFunc x2 x1)) (tm, ty)
  pure (cxt', (vy, TyFunc x1 x2) : cs)
-- id_l
infer ((TmId, vy) : cxt) (tm, ty) = do
  tell [infoLog $ "id_l: " ++ judgement ((TmId, vy) : cxt) (tm, ty)]
  x <- TyVar <$> uvar
  (cxt', cs) <- infer cxt (tm, ty)
  pure (cxt', (vy, TyFunc x x) : cs)
-- unit_r
infer cxt (TmUnit, vy) = do
  tell [infoLog $ "unit_r: " ++ judgement cxt (TmUnit, vy)]
  pure (cxt, [(vy, TyUnit)])
-- inl_r
infer cxt (TmLeft tm1, vy) = do
  tell [infoLog $ "inl_r: " ++ judgement cxt (TmLeft tm1, vy)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt', cs) <- infer cxt (tm1, x1)
  pure (cxt', (vy, TySum x1 x2) : cs)
-- inr_r
infer cxt (TmRight tm2, vy) = do
  tell [infoLog $ "inr_r: " ++ judgement cxt (TmRight tm2, vy)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt', cs) <- infer cxt (tm2, x2)
  pure (cxt', (vy, TySum x1 x2) : cs)
-- tensor_r
infer cxt (TmTensor tm1 tm2, vy) = do
  tell [infoLog $ "tensor_r: " ++ judgement cxt (TmTensor tm1 tm2, vy)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt',  cs1) <- infer cxt (tm1, x1)
  (cxt'', cs2) <- infer cxt' (tm2, x2)
  pure (cxt'', (vy, TyTensor x1 x2) : (cs1 ++ cs2))
-- arrow_r
infer cxt (TmArrow tm1 tm2, vy) = do
  tell [infoLog $ "arrow_r: " ++ judgement cxt (TmArrow tm1 tm2, vy)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt', cs) <- infer (cxt # (tm1, x1)) (tm2, x2)
  pure (cxt', (vy, TyFunc x1 x2) : cs)
-- fold_r
infer cxt (TmFold (TyRec x ty) tm, vy) = do
  tell [infoLog $ "fold_r: " ++ judgement cxt (TmFold (TyRec x ty) tm, vy)]
  tell [infoLog $ "checkType " ++ printTree (TyRec x ty)]
  checkType [] (TyRec x ty)
  (cxt', cs) <- infer cxt (tm, (x ~> TyRec x ty) ty)
  pure (cxt', (vy, TyRec x ty) : cs)
-- trace_r
infer cxt (TmTrace ty tm, vy) = do
  tell [infoLog $ "trace_r: " ++ judgement cxt (TmTrace ty tm, vy)]
  tell [debugLog "checkType " ++ printTree ty]
  checkType [] ty -- tyに変数の重複がないか確認する必要あり
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt', cs) <- infer cxt (tm, TyFunc (TySum ty x1) (TySum ty x2))
  pure (cxt', (vy, TyFunc x1 x2) : cs)
-- lin_r
infer cxt (TmLin tm1 tm2, vy) = do
  tell [infoLog $ "lin_r: " ++ judgement cxt (TmLin tm1 tm2, vy)]
  x <- TyVar <$> uvar
  (cxt1, cs1) <- infer cxt (tm1, x)
  (cxt2, cs2) <- infer cxt (tm2, x)
  if cxt1 == cxt2 -- 🤔
    then pure (cxt1, (vy, x) : cs1 ++ cs2)
    else inferenceError Info "remainder contexts between lin are different."
-- comp_r
infer cxt (TmComp tm1 tm2, vy) = do
  tell [infoLog $ "comp_r: " ++ judgement cxt (TmComp tm1 tm2, vy)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  x3 <- TyVar <$> uvar
  (cxt',  cs1) <- infer cxt (tm1, TyFunc x1 x2)
  (cxt'', cs2) <- infer cxt' (tm2, TyFunc x2 x3)
  pure (cxt'', (vy, TyFunc x1 x3) : (cs1 ++ cs2))
-- flip_r
infer cxt (TmFlip ty, vy) = do
  tell [infoLog $ "flip_r: " ++ judgement cxt (TmFlip ty, vy)]
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  (cxt', cs) <- infer cxt (ty, TyFunc x2 x1)
  pure (cxt', (vy, TyFunc x1 x2) : cs)
-- id_r
infer cxt (TmId, vy) = do
  tell [infoLog $ "id_r: " ++ judgement cxt (TmId, vy)]
  x <- TyVar <$> uvar
  pure ([], [(vy, TyFunc x x)])
-- fail
infer cxt (tm, ty) = do
  tell [infoLog $ "fail: " ++ judgement cxt (tm, ty)]
  inferenceError Info $ "no inference rules matched to" ++ judgement cxt (tm,ty)

inferExpr :: (Expr, Type) -> Check [Constraint]
inferExpr (ExTerm tm, vy) = do
  (cxt, cs) <- infer [] (tm, vy)
  if null cxt
    then pure cs
    else inferenceError Info "context must be empty after type inference."
inferExpr (ExApp ex tm, vy) = do
  x1 <- TyVar <$> uvar
  x2 <- TyVar <$> uvar
  cs1 <- inferExpr (ex, TyFunc x1 x2)
  (cxt, cs2) <- infer [] (tm, x1)
  if null cxt
    then pure $ (vy, x2) : (cs1 ++ cs2)
    else inferenceError Info "context must be empty after type inference."

--型変数（自由束縛にかかわらず全て）
tmvars :: Term -> S.Set Ident
tmvars (TmVar x) = S.singleton x
tmvars TmUnit = S.empty
tmvars (TmLeft tm) = tmvars tm
tmvars (TmRight tm) = tmvars tm
tmvars (TmTensor tm1 tm2) = tmvars tm1 `S.union` tmvars tm2
tmvars (TmArrow tm1 tm2) = tmvars tm1 `S.union` tmvars tm2
tmvars (TmFold ty tm) = tmvars tm
tmvars (TmLin tm1 tm2) = tmvars tm1 `S.union` tmvars tm2
tmvars (TmTrace ty tm) = tmvars tm
tmvars (TmComp tm1 tm2) = tmvars tm1 `S.union` tmvars tm2
tmvars (TmFlip tm) = tmvars tm
tmvars TmEmpty = S.empty
tmvars TmId = S.empty

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
tmFV (TmLin tm1 tm2) = do -- Linは左右で同じ自由変数を持つこと
  tmFV1 <- tmFV tm1
  tmFV2 <- tmFV tm2
  if tmFV1 == tmFV2
    then pure tmFV1
    else do
      tell [warnLog "tmFV have same free variables, but not same: ", printTree tm1, printTree tm2]
      throwError "tmFV have same free variables, but not same"
tmFV (TmTrace _ tm) = tmFV tm
tmFV (TmComp tm1 tm2) = do
  tmFV1 <- tmFV tm1
  tmFV2 <- tmFV tm2
  pure $ tmFV1 ++ tmFV2
tmFV (TmFlip tm) = tmFV tm
tmFV TmEmpty = pure []
tmFV TmId = pure []