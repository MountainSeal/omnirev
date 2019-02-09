-- |与えられたomnirevの構文の型が正しく型付けできているか検査するためのモジュール群
module Omnirev.TypeChecker where

import Omnirev.AbsOmnirev
import Omnirev.ErrM
import Data.Map as Map

type Result = Err String
type Context a = Map String a
type Iso = (Func, Type, Type)


-- |Entrypoint of type checking 
check :: Program -> String
check p = case checkProg p of
  Ok  s -> s
  Bad s -> s

-- |Syntax sugar for type checking failure
failure :: Show a => a -> Result
failure x = Bad $ "type check error: " ++ show x


-- |The `purify` function replace type variables.
purify :: Type -> Context Type -> Maybe Type
purify t cxt =
  case t of
    TUnit ->
      Just TUnit
    TTensor t1 t2 ->
      case (purify t1 cxt, purify t2 cxt) of
        (Just t1', Just t2') -> Just $ TTensor t1' t2'
        _                    -> Nothing
    TSum t1 t2 ->
      case (purify t1 cxt, purify t2 cxt) of
        (Just t1', Just t2') -> Just $ TSum t1' t2'
        _                    -> Nothing
    TStar t ->
      case purify t cxt of
        Just t' -> Just $ TStar t'
    TVar (Ident str) ->
      case Map.lookup str cxt of
        Just t  -> purify t cxt
        Nothing -> Nothing


checkProg :: Program -> Result
checkProg (Prog defs) =
  case defs of
    [] -> failure "there is no program."
    _  -> checkDefs defs (empty, empty)


checkDefs :: [Def] -> (Context Type, Context Iso) -> Result
checkDefs [] _ = Ok "Success!"
checkDefs (d:ds) (tcxt, fcxt) =
  case res of
    Ok  _ -> checkDefs ds (tcxt', fcxt')
    Bad x -> failure x
  where
    (tcxt', fcxt', res) = checkDef d (tcxt, fcxt)


checkDef :: Def -> (Context Type, Context Iso) -> (Context Type, Context Iso, Result)
checkDef def (tcxt, fcxt) = case def of
  DType (Ident str) t ->
    case Map.lookup str tcxt of
      Just t' ->
        (tcxt, fcxt, Bad $ "there is definition conflict: " ++ str)
      Nothing ->
        case checkType t tcxt of
          Ok  o -> (Map.insert str t tcxt, fcxt, Ok o)
          Bad x -> (tcxt, fcxt, Bad x)

  DFunc (Ident str) dom cod f ->
    case Map.lookup str fcxt of
      Just (f', dom', cod') ->
        (tcxt, fcxt, Bad $ "there is definition conflict: " ++ str)
      Nothing ->
        case (purify dom tcxt, purify cod tcxt) of
          (Just dom', Just cod') ->
            case checkFunc f (dom', cod') (tcxt, fcxt) of
              Ok  o -> (tcxt, Map.insert str (f, dom, cod) fcxt, Ok o)
              Bad x -> (tcxt, fcxt, Bad x)
          _ ->
            (tcxt, fcxt, Bad $ "type" ++ str ++ "not found")


checkType :: Type -> Context Type -> Result
checkType t cxt = case t of
  TUnit ->
    Ok ""

  TTensor t1 t2 ->
    case (res1, res2) of
      (Ok _, Ok _)     -> Ok ""
      (Ok _, Bad s2)   -> Bad $ "right side type of a tensor is something wrong.\n" ++ s2
      (Bad s1, Ok _)   -> Bad $ "left side type of a tensor is something wrong.\n" ++ s1
      (Bad s1, Bad s2) -> Bad $ "both side type of a tensor is something wrong.\n" ++ s1 ++ s2
    where
      res1 = checkType t1 cxt
      res2 = checkType t2 cxt

  TSum t1 t2 ->
    case (res1, res2) of
      (Ok _, Ok _)     -> Ok ""
      (Ok _, Bad s2)   -> Bad $ "right side type of a sum is something wrong.\n" ++ s2
      (Bad s1, Ok _)   -> Bad $ "left side type of a sum is something wrong.\n" ++ s1
      (Bad s1, Bad s2) -> Bad $ "both side type of a sum is something wrong.\n" ++ s1 ++ s2
    where
      res1 = checkType t1 cxt
      res2 = checkType t2 cxt

  TStar t ->
    checkType t cxt

  TVar (Ident str) ->
    case Map.lookup str cxt of
      Just _  -> Ok ""
      Nothing -> Bad $ "not found type variable " ++ str ++ ".\n"


checkFunc :: Func -> (Type, Type) -> (Context Type, Context Iso) -> Result
checkFunc f (dom, cod) (tcxt, fcxt) = case f of
  FId ->
    if dom == cod
    then Ok ""
    else Bad "id function must have same domain and codomain.\n"

  FComp f1 f2 ->
    case searchCodomain (FComp f1 f2) dom fcxt of
      Just _ -> Ok ""
      Nothing -> Bad "composite function must have same codomain of left side function and domain of right side function.\n"

  FTensor f1 f2 ->
    case (dom, cod) of
      (TTensor t1 t2, TTensor t1' t2') ->
        case (res1, res2) of
          (Ok _, Ok _)     -> Ok ""
          (Ok _, Bad s2)   -> Bad $ "right side function of tensor is something wrong.\n" ++ s2
          (Bad s1, Ok _)   -> Bad $ "left side function of tensor is something wrong.\n" ++ s1
          (Bad s1, Bad s2) -> Bad $ "both side function of tensor is something wrong.\n" ++ s1 ++ s2
        where
          res1 = checkFunc f1 (t1, t1') (tcxt, fcxt)
          res2 = checkFunc f2 (t2, t2') (tcxt, fcxt)
      _ -> Bad "invalid domain or codomain of tensor function.\n"

  FTensUnit ->
    case (dom, cod) of
      (TTensor TUnit t, t') -> if t == t'
        then Ok ""
        else Bad "invalid domain or codomain of unit_*.\n"
      _ -> Bad "invalid domain or codomain of unit_*.\n"

  FTensAssoc ->
    case (dom, cod) of
      (TTensor t1 (TTensor t2 t3), TTensor (TTensor t1' t2') t3') ->
        if t1 == t1' && t2 == t2' && t3 == t3'
        then Ok ""
        else Bad "invalid domain or codomain of assoc_*.\n"
      _ -> Bad "invalid domain or codomain of assoc_*.\n"

  FTensSym ->
    case (dom, cod) of
      (TTensor t1 t2, TTensor t2' t1') ->
        if t1 == t1' && t2 == t2'
        then Ok ""
        else Bad "invalid domain or codomain of sym_*.\n"
      _ -> Bad "invalid domain or codomain of sym_*.\n"

  FSum f1 f2 ->
    case (dom, cod) of
      (TSum t1 t2, TSum t1' t2') ->
        case (res1, res2) of
          (Ok _, Ok _)     -> Ok ""
          (Ok _, Bad s2)   -> Bad $ "both side function of sum is something wrong.\n" ++ s2
          (Bad s1, Ok _)   -> Bad $ "both side function of sum is something wrong.\n" ++ s1
          (Bad s1, Bad s2) -> Bad $ "both side function of sum is something wrong.\n" ++ s1 ++ s2
        where
          res1 = checkFunc f1 (t1, t1') (tcxt, fcxt)
          res2 = checkFunc f2 (t2, t2') (tcxt, fcxt)
      _ -> Bad "invalid domain or codomain of sum function.\n"

  FSumAssoc ->
    case (dom, cod) of
      (TSum t1 (TSum t2 t3), TSum (TSum t1' t2') t3') ->
        if t1 == t1' && t2 == t2' && t3 == t3'
        then Ok ""
        else Bad "invalid domain or codomain of assoc_+.\n"
      _ -> Bad "invalid domain or codomain of assoc_+.\n"

  FSumSym ->
    case (dom, cod) of
      (TSum t1 t2, TSum t2' t1') ->
        if t1 == t1' && t2 == t2'
        then Ok ""
        else Bad "invalid domain or codomain of sym_+.\n"
      _ -> Bad "invalid domain or codomain of sym_+.\n"

  FDistrib ->
    case (dom, cod) of
      (TTensor (TSum t1 t2) t3, TSum (TTensor t1' t3') (TTensor t2' t3'')) ->
        if t1 == t1' && t2 == t2' && t3 == t3' && t3' == t3''
        then Ok ""
        else Bad "invalid domain or codomain of distrib.\n"
      _ -> Bad "invalid domain or codomain of distrib.\n"

  FEval t ->
    case purify t tcxt of
      Just t' ->
        case (dom, cod) of
          (TTensor t'' (TStar t'''), TUnit) ->
            if t' == t'' && t'' == t'''
            then Ok ""
            else Bad "invalid domain or codomain of eval.\n"
          _ ->
            Bad "invalid domain or codomain of eval.\n"
      Nothing ->
        Bad $ "type" ++ show t ++ "not found."

  FDagger f ->
    checkFunc f (cod, dom) (tcxt, fcxt)

  FVar (Ident str) ->
    case Map.lookup str fcxt of
      Just _  -> Ok ""
      Nothing -> Bad $ "not found type variable " ++ str ++ ".\n"

  FShift _ ->
    Ok ""


searchCodomain :: Func -> Type -> Context Iso -> Maybe Type
searchCodomain func domain cxt = case (func, domain) of
  (FId, t) ->
    Just t

  (FComp f1 f2, t1) ->
    case searchCodomain f1 t1 cxt of
      Just t2 -> searchCodomain f2 t2 cxt
      Nothing -> Nothing

  (FTensor f1 f2, TTensor t1 t2) ->
    case (maybe1, maybe2) of
      (Just t1', Just t2') ->
        Just (TTensor t1' t2')
      _ ->
        Nothing
    where
      maybe1 = searchCodomain f1 t1 cxt
      maybe2 = searchCodomain f2 t2 cxt

  (FTensUnit, TTensor TUnit t) ->
    Just t

  (FTensAssoc, TTensor t1 (TTensor t2 t3)) ->
    Just (TTensor (TTensor t1 t2) t3)

  (FTensSym, TTensor t1 t2) ->
    Just (TTensor t2 t1)

  (FSum f1 f2, TSum t1 t2) ->
    case (maybe1, maybe2) of
      (Just t1', Just t2') ->
        Just (TSum t1' t2')
      _ ->
        Nothing
    where
      maybe1 = searchCodomain f1 t1 cxt
      maybe2 = searchCodomain f2 t2 cxt

  (FSumAssoc, TSum t1 (TSum t2 t3)) ->
    Just (TSum (TSum t1 t2) t3)

  (FSumSym, TSum t1 t2) ->
    Just (TSum t2 t1)

  (FDistrib, TTensor (TSum t1 t2) t3) ->
    Just (TSum (TTensor t1 t3) (TTensor t2 t3))

  (FEval t, TTensor t' (TStar t'')) ->
    if t == t' && t' == t''
    then Just TUnit
    else Nothing

  (FDagger f, t) ->
    searchDomain f t cxt

  (FVar (Ident str), t) ->
    case Map.lookup str cxt of
      Just (_, dom, cod) ->
        if t == dom
        then Just cod
        else Nothing
      Nothing ->
        Nothing

  (FShift _, t) ->
    Just t

  _ ->
    Nothing


searchDomain :: Func -> Type -> Context Iso -> Maybe Type
searchDomain func codomain cxt = case (func, codomain) of
  (FId, t) ->
    Just t

  (FComp f1 f2, t3) ->
    case searchDomain f2 t3 cxt of
      Just t2 -> searchDomain f1 t2 cxt
      Nothing -> Nothing

  (FTensor f1 f2, TTensor t1 t2) ->
    case (maybe1, maybe2) of
      (Just t1', Just t2') ->
        Just (TTensor t1' t2')
      _ ->
        Nothing
    where
      maybe1 = searchDomain f1 t1 cxt
      maybe2 = searchDomain f2 t2 cxt

  (FTensUnit, t) ->
    Just (TTensor TUnit t)

  (FTensAssoc, TTensor (TTensor t1 t2) t3) ->
    Just (TTensor t1 (TTensor t2 t3))

  (FTensSym, TTensor t1 t2) ->
    Just (TTensor t2 t1)

  (FSum f1 f2, TSum t1 t2) ->
    case (maybe1, maybe2) of
      (Just t1', Just t2') ->
        Just (TSum t1' t2')
      _ ->
        Nothing
    where
      maybe1 = searchDomain f1 t1 cxt
      maybe2 = searchDomain f2 t2 cxt

  (FSumAssoc, TSum (TSum t1 t2) t3) ->
    Just (TSum t1 (TSum t2 t3))

  (FSumSym, TSum t1 t2) ->
    Just (TSum t2 t1)

  (FDistrib, TSum (TTensor t1 t3) (TTensor t2 t3')) ->
    if t3 == t3'
    then Just (TTensor (TSum t1 t2) t3)
    else Nothing

  (FEval t, TUnit) ->
    Just (TTensor t (TStar t))

  (FDagger f, t) ->
    searchCodomain f t cxt

  (FVar (Ident str), t) ->
    case Map.lookup str cxt of
      Just (_, dom, cod) ->
        if t == cod
        then Just dom
        else Nothing
      Nothing ->
        Nothing

  (FShift _, t) ->
    Just t

  _ ->
    Nothing
