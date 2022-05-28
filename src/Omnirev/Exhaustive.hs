-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Author: MountainSeal
module Omnirev.Exhaustive where

import Data.Map as M
import Omnirev.AbsOmnirev
import Omnirev.CheckOmnirev
import Omnirev.ErrM
import Prelude hiding (subtract)
import qualified Data.Set as S
import Control.Monad.State

data Space
  = SpEmpty
  | SpType Type
  | SpSum Space Space
  | SpKonst String [Space]
  deriving (Eq, Ord, Show, Read)


exhaust :: Term -> Type -> Bool
exhaust trm typ =
  case ioput trm typ of
    Just (spIn, spOut) -> equalToEmpty $ subtract spIn spOut
    Nothing -> False


-- 与えられた(関数)項の入力部分と出力部分をスペースの組にして返す
ioput :: Term -> Type -> Maybe (Space, Space)
ioput trm typ = evalState (ioput' trm typ) (tmvars trm)

ioput' :: Term -> Type -> State (S.Set Ident) (Maybe (Space, Space))
ioput' (TmArrow tmIn tmOut) (TyFunc tyIn tyOut) =
  pure $ Just (project tmIn tyIn, project tmOut tyOut)
ioput' (TmTrace ty tm)      (TyFunc tyIn tyOut) =
  ioput' tm $ TyFunc (TySum ty tyIn) (TySum ty tyOut)
ioput' (TmLin tm1 tm2)      (TyFunc tyIn tyOut) = do
  mbsp1 <- ioput' tm1 $ TyFunc tyIn tyOut
  mbsp2 <- ioput' tm2 $ TyFunc tyIn tyOut
  pure $ case (mbsp1, mbsp2) of
    (Just (spIn1, spOut1), Just (spIn2, spOut2)) -> Just (SpSum spIn1 spIn2, SpSum spOut1 spOut2)
    _ -> Nothing
ioput' (TmComp tm1 tm2)     (TyFunc tyIn tyOut) =
  case checkComposition (TmComp tm1 tm2) (TyFunc tyIn tyOut) of
    Bad s -> pure Nothing
    Ok ty -> do
      mbsp1 <- ioput' tm1 $ TyFunc tyIn ty
      mbsp2 <- ioput' tm2 $ TyFunc ty tyOut
      pure $ case (mbsp1, mbsp2) of
        (Just (spIn, _), Just (_, spOut)) -> Just (spIn, spOut)
        _ -> Nothing
ioput' TmEmpty            (TyFunc tyIn tyOut) =
  pure $ Just (SpEmpty, SpEmpty)
ioput' TmId               (TyFunc tyIn tyOut) = do -- TmIdを受け取った場合は，自由変数を探してそれを束縛した関数項を返す（x ~> x）
  vars <- get
  let v = nextuvar vars 0
  modify $ S.insert v
  pure $ Just (project (TmVar v) tyIn, project (TmVar v) tyOut)
  where
    var n = Ident $ "x_" ++ show n
    nextuvar e n = if S.member (var n) e
      then nextuvar e (n+1)
      else var n
ioput' (TmFlip tm)          (TyFunc tyIn tyOut) =
  ioput' tm (TyFunc tyOut tyIn)
ioput' _ _ = pure Nothing


-- 型付き項をスペースへ射影する
project :: Term -> Type -> Space
project (TmVar x)           ty                = SpType ty
project  TmUnit             TyUnit            = SpKonst "unit" []
project (TmLeft tm)        (TySum ty1 ty2)    = SpKonst "inl" [project tm ty1]
project (TmRight tm)       (TySum ty1 ty2)    = SpKonst "inr" [project tm ty2]
project (TmTensor tm1 tm2) (TyTensor ty1 ty2) = SpKonst "tensor" [project tm1 ty1, project tm2 ty2]
project (TmArrow tm1 tm2)  (TyFunc ty1 ty2)   = SpKonst "arrow" [project tm1 ty1, project tm2 ty2]
project (TmFold _ tm)      (TyRec x ty)       = SpKonst "fold" [project tm $(x ~> TyRec x ty)ty]
project (TmTrace uy tm)    (TyFunc ty1 ty2)   = project tm (TyFunc (TySum uy ty1) (TySum uy ty2))
project  TmEmpty            ty                = SpEmpty
project (TmLin tm1 tm2)     ty                = SpSum (project tm1 ty) (project tm2 ty)
project  TmId              (TyFunc ty1 ty2)   = SpKonst "arrow" [SpType ty1, SpType ty2]
-- 中間の型の情報が無いと判断が出来ないので，タグのような形で各項に対して型情報を与える必要があるかも
-- そうすれば，いちいち再度型を走査する必要もなくなるヵな？
project (TmComp tm1 tm2)   (TyFunc ty1 ty2)   =
  case checkComposition (TmComp tm1 tm2) (TyFunc ty1 ty2) of
    Bad s -> SpEmpty -- そもそも型付けされている前提なのでこのケースは起こりえないが，一応空とする
    Ok ty -> SpKonst "arrow" [project tm1 (TyFunc ty1 ty), project tm2 (TyFunc ty ty2)]
project (TmFlip tm)        (TyFunc ty1 ty2)   = project tm (TyFunc ty2 ty1)
project _ _ = SpEmpty


-- 与えられた型tyをコンストラクターの空間に分解する
-- なぜこんな面倒なことをするかというと，projectですぐに分解してしまうと，再帰型の場合に停止しないため
decompose :: Type -> Maybe Space
decompose (TyVar x) = Nothing
decompose TyUnit = Just $ SpKonst "unit" []
decompose (TySum ty1 ty2) = Just $ SpSum (SpKonst "inl" [SpType ty1]) (SpKonst "inr" [SpType ty2])
decompose (TyTensor ty1 ty2) = Just $ SpKonst "tensor" [SpType ty1, SpType ty2]
decompose (TyFunc ty1 ty2) = Just $ SpKonst "arrow" [SpType ty1, SpType ty2]
decompose (TyRec x ty) = Just $ SpKonst "fold" [SpType $(x ~> TyRec x ty) ty]


intersect :: Space -> Space -> Space
intersect SpEmpty _ = SpEmpty
intersect _ SpEmpty = SpEmpty
intersect (SpSum sp1 sp2) sp = SpSum (sp1 `intersect` sp) (sp2 `intersect` sp)
intersect sp (SpSum sp1 sp2) = SpSum (sp `intersect` sp1) (sp `intersect` sp2)
intersect (SpType ty1) (SpType ty2) = if ty1 `tyEquiv` ty2
  then SpType ty1
  else case (decompose ty1, decompose ty2) of -- 型が同じではない場合（つまり型付けされていない場合）の処理
    (Just sp1, Just sp2) -> sp1 `intersect` sp2
    _ -> SpEmpty
intersect (SpType ty) (SpKonst knst params) = -- SpKonst knst params
  case decompose ty of
    Just sp -> sp `intersect` SpKonst knst params
    Nothing -> SpEmpty -- 型変数の場合しかないので，型付けされていない場合に当たる．一旦空を返す
intersect (SpKonst knst params) (SpType ty) = -- SpKonst knst params
  case decompose ty of
    Just sp -> sp `intersect` SpKonst knst params
    Nothing -> SpEmpty -- 型変数の場合しかないので，型付けされていない場合に当たる．一旦空を返す
intersect (SpKonst knst1 params1) (SpKonst knst2 params2)
  | knst1 == knst2 = SpKonst knst1 $ zipWith intersect params1 params2
  | otherwise = SpEmpty


subtract :: Space -> Space -> Space
subtract SpEmpty _ = SpEmpty
subtract sp SpEmpty = sp
subtract (SpSum sp1 sp2) sp = SpSum (subtract sp1 sp) (subtract sp2 sp)
subtract sp (SpSum sp1 sp2) = subtract (subtract sp sp1) sp2
subtract (SpType ty1) (SpType ty2) = if ty1 `tyEquiv` ty2
  then SpEmpty
  else case (decompose ty1, decompose ty2) of
    (Just sp1, Just sp2) -> sp1 `intersect` sp2
    _ -> SpEmpty -- どちらかが型変数の場合しかないので，型付けされていない場合に当たる．型変数はマッチしない扱いとする．
subtract (SpType ty) (SpKonst knst params) = case decompose ty of
  Just sp -> subtract sp (SpKonst knst params)
  Nothing -> SpType ty -- どちらかが型変数の場合しかないので，型付けされていない場合に当たる．型変数はマッチしない扱いとする．
subtract (SpKonst knst params) (SpType ty) = case decompose ty of
  Just sp -> subtract (SpKonst knst params) sp
  Nothing -> SpEmpty -- どちらかが型変数の場合しかないので，型付けされていない場合に当たる．型変数はマッチしない扱いとする．
subtract (SpKonst knst1 params1) (SpKonst knst2 params2) -- 同じコンストラクタの時とそれ以外で場合分けが必要
  | knst1 /= knst2 = SpEmpty
  | all equalToEmpty $ zipWith subtract  params1 params2 = SpEmpty
  | any equalToEmpty $ zipWith intersect params1 params2 = SpKonst knst1 params1
  | otherwise = case (params1, params2) of
    ([], []) -> SpEmpty
    ([s1], [w1]) -> SpKonst knst1 [subtract s1 w1]
    ([s1,s2], [w1,w2]) -> SpSum (SpKonst knst1 [subtract s1 w1, s2]) (SpKonst knst1 [s1, subtract s2 w2])
    _ -> SpEmpty


equalToEmpty :: Space -> Bool
equalToEmpty  SpEmpty              = True
equalToEmpty (SpType ty)           = maybe False equalToEmpty (decompose ty)
equalToEmpty (SpSum s1 s2)         = equalToEmpty s1 && equalToEmpty s2
equalToEmpty (SpKonst knst params) = any equalToEmpty params