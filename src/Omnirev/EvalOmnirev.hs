{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Omnirev.EvalOmnirev(eval, evalEval, evalExpr) where


import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Applicative( (<*>) )
import Data.List
import qualified Data.Map as M
import Omnirev.AbsOmnirev
import Omnirev.PrintOmnirev
import Omnirev.CheckOmnirev
import Omnirev.ErrM
import Omnirev.Log
import Omnirev.Env


newtype Eval a = Eval (ReaderT (Env Term) (StateT (Env Alias) (ExceptT String (WriterT Logs Identity))) a)
  deriving (Functor, Applicative, Monad, MonadWriter Logs, MonadError String, MonadState (Env Alias), MonadReader (Env Term))

-- TO DO LIST
-- Exprを評価して結果で置き換える

runEval :: Eval a -> Env Alias -> (Either String a, Logs)
runEval (Eval m) env = runIdentity (runWriterT (runExceptT (evalStateT (runReaderT m M.empty) env)))

evalEval :: Eval a -> Env Alias -> Either String a
evalEval (Eval m) env = fst $ runEval (Eval m) env


-- 各型，項，式の実行結果をログと共に返す
eval :: ([Ident], Env Alias) -> Err ([(Ident, Term)], Logs)
eval (is,env) = case runEval (evalIdents is) env of
  (Left  str, logs) -> Bad $ unlines logs
  (Right res, logs) -> Ok (res, logs)

evalIdents :: [Ident] -> Eval [(Ident, Term)]
evalIdents [] = pure []
evalIdents (i:is) = do
  tell [infoLog "eval start"]
  (_, tm) <- evalIdent i

  let (Ident s) = i
  mb <- M.lookup s <$> get
  case mb of
    Just (ATerm _ ty) -> modify $ M.insert s (ATerm tm ty)
    Just (AExpr _ ty) -> modify $ M.insert s (ATerm tm ty)
    Just AType{} -> throwError ""
    Just AVar{} -> throwError ""
    Nothing -> throwError ""

  tms <- evalIdents is
  tell [infoLog "eval finish"]
  pure $ (i,tm):tms

evalIdent :: Ident -> Eval (Ident, Term)
evalIdent (Ident s) = do
  env <- get
  case M.lookup s env of
    Nothing -> do
      tell [errorLog $ printTree s ++ "not found in environment"]
      throwError $ printTree s ++ "not found in environment"
    Just AType{} -> do
      tell [errorLog "something went wrong"]
      throwError "something went wrong"
    Just ATerm{} -> do
      tell [errorLog "something went wrong"]
      throwError "something went wrong"
    Just (AExpr ex _) -> do
      tell [infoLog $ "eval expr            : " ++ printTree s ++ " = " ++ printTree ex]
      tm <- evalExpr ex
      tell [infoLog $ "result expr          : " ++ printTree s ++ " = " ++ printTree tm]
      pure (Ident s,tm)
    Just AVar{} -> do
      tell [errorLog "something went wrong"]
      throwError "something went wrong"

evalExpr :: Expr -> Eval Term
evalExpr (ExTerm tm) = do
  tell [infoLog "conversion"]
  conv tm
evalExpr (ExApp ex tm) = do
  um <- evalExpr ex
  tell [infoLog $ "application : " ++ printTree (ExApp (ExTerm um) tm)]
  appl um tm

-- 以降から実際の項の評価処理を書く

-- 項の変数から項へのコンテキスト
type Sbst = Maybe (M.Map Ident Term)

context :: Sbst -> String
context sb = case sb of
  Nothing  -> "_|_"
  Just cxt -> "{" ++ intercalate "," (map mp $ M.toList cxt) ++ "}"
    where
      mp :: (Ident, Term) -> String
      mp (i, tm) = printTree i ++ "|->" ++ printTree tm

unionAnd :: Sbst -> Sbst -> Sbst
unionAnd (Just m1) (Just m2) = Just $ M.union m1 m2
unionAnd (Just m1) Nothing   = Nothing
unionAnd Nothing   (Just m2) = Nothing
unionAnd Nothing   Nothing   = Nothing

unionOr :: Sbst -> Sbst -> Sbst
unionOr (Just m1) (Just m2) = Just $ M.union m1 m2
unionOr (Just m1) Nothing   = Just m1
unionOr Nothing   (Just m2) = Just m2
unionOr Nothing   Nothing   = Nothing

-- matching
mtch :: Term -> Term -> Eval Sbst
mtch (TmVar x) tm =
  pure $ Just $ M.singleton x tm
mtch TmUnit TmUnit =
  pure $ Just M.empty
mtch (TmLeft tm) (TmLeft um) =
  mtch tm um
mtch (TmLeft tm) (TmRight um) =
  pure Nothing
mtch (TmRight tm) (TmLeft um) =
  pure Nothing
mtch (TmRight tm) (TmRight um) =
  mtch tm um
mtch (TmTensor tm1 tm2) (TmTensor um1 um2) = do
  sb1 <- mtch tm1 um1
  sb2 <- mtch tm2 um2
  pure $ unionAnd sb1 sb2
mtch (TmArrow tm1 tm2) (TmArrow um1 um2) = do
  sb1 <- mtch tm1 um1
  sb2 <- mtch tm2 um2
  pure $ unionAnd sb1 sb2
mtch (TmFold ty tm) (TmFold uy um) =
  mtch tm um
mtch (TmTrace ty tm) (TmTrace uy um) =
  mtch tm um
mtch tm (TmLin um1 um2) = do
  sb1 <- mtch tm um1
  sb2 <- mtch tm um2
  pure $ unionOr sb1 sb2
mtch (TmLin tm1 tm2) um = do
  sb1 <- mtch tm1 um
  sb2 <- mtch tm2 um
  pure $ unionOr sb1 sb2
mtch (TmComp tm1 tm2) (TmComp um1 um2) = do
  sb1 <- mtch tm1 um1
  sb2 <- mtch tm2 um2
  pure $ unionAnd sb1 sb2
mtch (TmComp (TmVar x) tm) um =
  mtch (TmVar x) (TmComp um (TmFlip tm))
mtch (TmComp tm (TmVar x)) um =
  mtch (TmVar x) (TmComp (TmFlip tm) um)
mtch (TmFlip tm) um =
  mtch tm um
mtch TmId TmId =
  pure $ Just M.empty
mtch TmEmpty um =
  pure $ Just M.empty
mtch tm TmEmpty =
  pure $ Just M.empty
mtch _ _ =
  throwError ""

-- substitution
sbst :: Sbst -> Term -> Eval (Term, Sbst)
sbst (Just m) (TmVar x) =
  case M.lookup x m of
    Nothing -> pure (TmEmpty, Just m)
    Just tm -> pure (tm, Just $ M.delete x m)
sbst (Just m) TmUnit =
  pure (TmUnit, Just m)
sbst (Just m) (TmLeft tm1) = do
  (tm1', sb') <- sbst (Just m) tm1
  pure (TmLeft tm1', sb')
sbst (Just m) (TmRight tm2) = do
  (tm2', sb') <- sbst (Just m) tm2
  pure (TmRight tm2', sb')
sbst (Just m) (TmTensor tm1 tm2) = do
  (tm1', sb')  <- sbst (Just m) tm1
  (tm2', sb'') <- sbst sb' tm2
  pure (TmTensor tm1' tm2', sb'')
sbst (Just m) (TmArrow tm1 tm2) = do
  (tm1', sb')  <- sbst (Just m) tm1
  (tm2', sb'') <- sbst sb' tm2
  pure (TmArrow tm1' tm2', sb'')
sbst (Just m) (TmFold ty tm) = do
  (tm', sb') <- sbst (Just m) tm
  pure (TmFold ty tm', sb')
sbst (Just m) (TmTrace ty tm) = do
  (tm', sb') <- sbst (Just m) tm
  pure (TmTrace ty tm', sb')
sbst (Just m) (TmLin tm1 tm2) = do
  (tm1', sb')  <- sbst (Just m) tm1
  (tm2', sb'') <- sbst sb' tm2
  pure (TmLin tm1' tm2', sb'')
sbst (Just m) (TmComp tm1 tm2) = do
  (tm1', sb')  <- sbst (Just m) tm1
  (tm2', sb'') <- sbst sb' tm2
  pure (TmComp tm1' tm2', sb'')
sbst (Just m) TmId =
  pure (TmId , Just m)
sbst (Just m) TmEmpty =
  pure (TmEmpty, Just m)
sbst (Just m) (TmFlip tm) = do
  (tm', sb') <- sbst (Just m) tm
  pure (TmFlip tm', sb')
sbst Nothing tm =
  pure (TmEmpty, Nothing)


-- conversion
-- 冪等律と対称律だけどうすればよいか思いつかない
-- 射の足し算と掛け算は入れ子の場合があるので，doで中身についてもconv関数を通させる
conv :: Term -> Eval Term
conv (TmVar x) = do
  tell [debugLog $ "var            : " ++ printTree (TmVar x)]
  pure $TmVar x
conv TmUnit = do
  tell [debugLog $ "unit           : " ++ printTree TmUnit]
  pure TmUnit
conv (TmLeft tm) = do
  tm' <- conv tm
  case tm' of
    TmEmpty -> do
      tell [debugLog $ "emp_inl        : " ++ printTree (TmLeft tm)]
      pure TmEmpty
    TmLin tm1' tm2' -> do
      tell [debugLog $ "lin_inl        : " ++ printTree (TmLeft tm)]
      pure $ TmLin (TmLeft tm1') (TmLeft tm2')
    _ -> do
      tell [debugLog $ "inl            : " ++ printTree (TmLeft tm)]
      pure $ TmLeft tm'
conv (TmRight tm) = do
  tm' <- conv tm
  case tm' of
    TmEmpty -> do
      tell [debugLog $ "emp_inr        : " ++ printTree (TmRight tm)]
      pure TmEmpty
    TmLin tm1' tm2' -> do
      tell [debugLog $ "lin_inr        : " ++ printTree (TmRight tm)]
      pure $ TmLin (TmRight tm1') (TmRight tm2')
    _ -> do
      tell [debugLog $ "inr            : " ++ printTree (TmRight tm)]
      pure $ TmRight tm'
conv (TmTensor tm1 tm2) = do
  tm1' <- conv tm1
  tm2' <- conv tm2
  case (tm1', tm2') of
    (TmEmpty, _) -> do
      tell [debugLog $ "emp_l_tensor   : " ++ printTree (TmTensor tm1 tm2)]
      pure TmEmpty
    (_, TmEmpty) -> do
      tell [debugLog $ "emp_r_tensor   : " ++ printTree (TmTensor tm1 tm2)]
      pure TmEmpty
    (TmLin tm11' tm12', _) -> do
      tell [debugLog $ "lin_l_tensor   : " ++ printTree (TmTensor tm1 tm2)]
      pure $ TmLin (TmTensor tm11' tm2') (TmTensor tm12' tm2')
    (_, TmLin tm21' tm22') -> do
      tell [debugLog $ "emp_r_tensor   : " ++ printTree (TmTensor tm1 tm2)]
      pure $ TmLin (TmTensor tm1' tm21') (TmTensor tm1' tm22')
    (_, _) -> do
      tell [debugLog $ "tensor         : " ++ printTree (TmTensor tm1 tm2)]
      pure $ TmTensor tm1' tm2'
conv (TmArrow tm1 tm2) = do
  tm1' <- conv tm1
  tm2' <- conv tm2
  case (tm1', tm2') of
    (TmEmpty, _) -> do
      tell [debugLog $ "emp_l_arrow    : " ++ printTree (TmArrow tm1 tm2)]
      pure TmEmpty
    (_, TmEmpty) -> do
      tell [debugLog $ "emp_r_arrow    : " ++ printTree (TmArrow tm1 tm2)]
      pure TmEmpty
    (TmLin tm11' tm12', _) -> do
      tell [debugLog $ "lin_l_arrow    : " ++ printTree (TmArrow tm1 tm2)]
      pure $ TmLin (TmArrow tm11' tm2') (TmArrow tm12' tm2')
    (_, TmLin tm21' tm22') -> do
      tell [debugLog $ "lin_r_arrow    : " ++ printTree (TmArrow tm1 tm2)]
      pure $ TmLin (TmArrow tm1' tm21') (TmArrow tm1' tm22')
    _ -> do
      tell [debugLog $ "arrow          : " ++ printTree (TmArrow tm1 tm2)]
      pure $ TmArrow tm1' tm2'
conv (TmFold ty tm) = do
  tm' <- conv tm
  case tm' of
    TmEmpty -> do
      tell [debugLog $ "emp_fold       : " ++ printTree (TmFold ty tm)]
      pure TmEmpty
    TmLin tm1' tm2' -> do
      tell [debugLog $ "lin_fold       : " ++ printTree (TmFold ty tm)]
      pure $ TmLin (TmFold ty tm1') (TmFold ty tm2')
    _ -> do
      tell [debugLog $ "fold           : " ++ printTree (TmFold ty tm)]
      pure $ TmFold ty tm'
conv (TmTrace ty tm) = do
  tm' <- conv tm
  case tm' of
    TmEmpty -> do
      tell [debugLog $ "emp_trace      : " ++ printTree (TmTrace ty tm)]
      pure TmEmpty
    tm' -> do
      tell [debugLog $ "trace          : " ++ printTree (TmTrace ty tm)]
      pure $ TmTrace ty tm'
conv (TmLin tm1 tm2) = do
  tm1' <- conv tm1
  tm2' <- conv tm2
  case (tm1', tm2') of
    (TmEmpty, _) -> do
      tell [debugLog $ "emp_l_linear   : " ++ printTree (TmLin tm1 tm2)]
      pure tm2'
    (_, TmEmpty) -> do
      tell [debugLog $ "emp_r_linear   : " ++ printTree (TmLin tm1 tm2)]
      pure tm1'
    (TmLin tm11' tm12', _) -> do
      tell [debugLog $ "lin_l_linear   : " ++ printTree (TmLin tm1 tm2)]
      pure $ TmLin tm11' (TmLin tm12' tm2')
    (_, TmLin tm21' tm22') -> do
      tell [debugLog $ "lin_r_linear   : " ++ printTree (TmLin tm1 tm2)]
      pure $ TmLin tm1' (TmLin tm21' tm22')
    _ -> do
      tell [debugLog $ "linear         : " ++ printTree (TmLin tm1 tm2)]
      pure $ TmLin tm1' tm2' -- 等しいかどうかで場合分けしたい
conv (TmComp tm1 tm2) = do
  tm1' <- conv tm1
  tm2' <- conv tm2
  case (tm1', tm2') of
    (TmEmpty, _) -> do
      tell [debugLog $ "emp_l_compose  : " ++ printTree (TmComp tm1 tm2)]
      pure TmEmpty
    (_, TmEmpty) -> do
      tell [debugLog $ "emp_r_compose  : " ++ printTree (TmComp tm1 tm2)]
      pure TmEmpty
    (TmId, _) -> do
      tell [debugLog $ "id_l_compose   : " ++ printTree (TmComp tm1 tm2)]
      pure tm2'
    (_, TmId) -> do
      tell [debugLog $ "id_r_compose   : " ++ printTree (TmComp tm1 tm2)]
      pure tm1'
    (TmLin tm11' tm12' , _) -> do
      
      tm112 <- conv $ TmComp tm11' tm2'
      tm122 <- conv $ TmComp tm12' tm2'
      tell [debugLog $ "lin_l_compose  : " ++ printTree (TmComp tm1 tm2)]
      pure $ TmLin tm112 tm122
      -- pure $ TmLin (TmComp tm11' tm2') (TmComp tm12' tm2') -- デバッグの結果，2つのcompそれぞれ改めてconv関数にかける必要がある
    (_, TmLin tm21' tm22' ) -> do
      tm121 <- conv $ TmComp tm1' tm21'
      tm122 <- conv $ TmComp tm1' tm22'
      tell [debugLog $ "lin_r_compose  : " ++ printTree (TmComp tm1 tm2)]
      pure $ TmLin tm121 tm122
      -- pure $ TmLin (TmComp tm1' tm21') (TmComp tm1' tm22')
    (TmComp tm11' tm12', _) -> do
      tell [debugLog $ "cmp_l_compose  : " ++ printTree (TmComp tm1 tm2)]
      pure $ TmComp tm11' (TmComp tm12' tm2')
    (_, TmComp tm21' tm22') -> do
      tell [debugLog $ "cmp_r_compose  : " ++ printTree (TmComp tm1 tm2)]
      pure $ TmComp tm1' (TmComp tm21' tm22')
    (TmArrow tm11 tm12 , TmArrow tm21 tm22 ) -> do
      tell [debugLog $ "arrw_compose   : " ++ printTree (TmComp tm1 tm2)]
      sb <- mtch tm21 tm12
      (tm11', sb1) <- sbst sb tm11
      (tm22', sb2) <- sbst sb tm22
      pure $ TmArrow tm11' tm22' -- sb1 sb2の環境が空になっているか検証したほうが良いかも
    _ -> do
      tell [debugLog $ "composition    : " ++ printTree (TmComp tm1 tm2)]
      pure $ TmComp tm1' tm2'
conv (TmFlip tm) =
  case tm of
    TmVar x -> do
      tell [debugLog $ "flip_var       : " ++ printTree (TmFlip tm)]
      pure $ TmFlip $ TmVar x
    TmUnit -> do
      tell [debugLog $ "flip_unit      : " ++ printTree (TmFlip tm)]
      pure TmUnit
    TmLeft tm' -> do
      tell [debugLog $ "flip_inl       : " ++ printTree (TmFlip tm)]
      TmLeft <$> conv (TmFlip tm')
    TmRight tm' -> do
      tell [debugLog $ "flip_inr       : " ++ printTree (TmFlip tm)]
      TmRight <$> conv (TmFlip tm')
    TmTensor tm1' tm2' -> do
      tell [debugLog $ "flip_tensor    : " ++ printTree (TmFlip tm)]
      TmTensor <$> conv (TmFlip tm1') <*> conv (TmFlip tm2')
    TmArrow tm1' tm2' -> do
      tell [debugLog $ "flip_arrow     : " ++ printTree (TmFlip tm)]
      TmArrow <$> conv tm2' <*> conv tm1'
    TmFold ty tm' -> do
      tell [debugLog $ "flip_fold      : " ++ printTree (TmFlip tm)]
      TmFold ty <$> conv (TmFlip tm')
    TmTrace ty tm' -> do
      tell [debugLog $ "flip_trace     : " ++ printTree (TmFlip tm)]
      TmTrace ty <$> conv (TmFlip tm')
    TmLin tm1' tm2' -> do
      tell [debugLog $ "flip_linear    : " ++ printTree (TmFlip tm)]
      TmLin <$> conv (TmFlip tm1') <*> conv (TmFlip tm2')
    TmComp tm1' tm2' -> do
      tell [debugLog $ "flip_compose   : " ++ printTree (TmFlip tm)]
      TmComp <$> conv (TmFlip tm2') <*> conv (TmFlip tm1')
    TmFlip tm' -> do
      tell [debugLog $ "flip_flip      : " ++ printTree (TmFlip tm)]
      conv tm'
    TmId -> do
      tell [debugLog $ "flip_id        : " ++ printTree (TmFlip tm)]
      pure TmId
    TmEmpty -> do
      tell [debugLog $ "flip_empty     : " ++ printTree (TmFlip tm)]
      pure TmEmpty
conv TmId = do
  tell [debugLog $ "id             : " ++ printTree TmId]
  pure TmId
conv TmEmpty = do
  tell [debugLog $ "empty          : " ++ printTree TmEmpty]
  pure TmEmpty


-- application
-- e @ t
appl :: Term -> Term -> Eval Term
appl (TmArrow tm1 tm2) tm = do
  tm1'        <- conv tm1
  tell [infoLog $ "pattern : " ++ printTree tm1']
  tm2'        <- conv tm2
  tell [infoLog $ "matcher : " ++ printTree tm2']
  tm'         <- conv tm
  tell [infoLog $ "input   : " ++ printTree tm']
  sb          <- mtch tm1' tm'
  tell [infoLog $ "context : " ++ context sb]
  (tm'', sb') <- sbst sb tm2'
  tell [infoLog $ "output  : " ++ printTree tm'']
  case sb' of
    Nothing ->
      pure tm''
    Just cxt ->
      if null cxt
        then pure tm''
        else do
          tell [errorLog "context not empty   : " ++ context sb']
          throwError $ "context not empty   : " ++ context sb'
appl TmEmpty tm = do
  tell [infoLog "empty application    : "]
  pure TmEmpty
appl TmId tm = do
  tell [infoLog "identity application : "]
  conv tm
appl (TmLin tm1 tm2) tm = do
  tell [infoLog "sepate at function"]
  tell [infoLog "application of left"]
  tm1' <- appl tm1 tm
  tell [infoLog "application of right"]
  tm2' <- appl tm2 tm
  tell [infoLog "conversion"]
  conv $ TmLin tm1' tm2'
appl tm (TmLin tm1 tm2) = do
  tell [infoLog "sepate at value"]
  tell [infoLog "application of left"]
  tm1' <- appl tm1 tm
  tell [infoLog "application of right"]
  tm2' <- appl tm2 tm
  tell [infoLog "conversion"]
  conv $ TmLin tm1' tm2'
appl (TmTrace ty tm) (TmLeft um) = do
  um' <- appl tm (TmLeft um)
  case um' of
    TmLeft  um'' -> do
      tell [debugLog "trace loop           : " ++ printTree um'']
      appl (TmTrace ty tm) (TmLeft um'')
    TmRight um'' -> do
      tell [debugLog "trace end            : " ++ printTree um'']
      pure um''
    _ -> do
      tell [errorLog "trace must inl or inr : " ++ printTree um']
      throwError $ "trace must inl or inr : " ++ printTree um'
appl (TmTrace ty tm) um = do
  um' <- appl tm (TmRight um)
  case um' of
    TmLeft  um'' -> do
      tell [debugLog "trace start          : " ++ printTree um'']
      appl (TmTrace ty tm) (TmLeft um'')
    TmRight um'' -> do
      tell [debugLog "trace nix            : " ++ printTree um'']
      pure um''
    _ -> do
      tell [errorLog "trace must inl or inr : " ++ printTree um']
      throwError $ "trace must inl or inr : " ++ printTree um'
appl _ _ = throwError ""
