module Omnirev.TypeChecker where

import Omnirev.AbsOmnirev
import Omnirev.ErrM
import Data.Map as Map hiding (update)

type Result = Err String
type Context a = Map String a
type Iso = (Func, Type, Type)



check :: Program -> String
check p = case checkProgram p of
  Ok s -> s
  Bad s -> s

failure :: Show a => a -> Result
failure x = Bad $ "type check error: " ++ show x


update :: String -> a -> Context a -> Context a
update str d cxt = case (Map.lookup str cxt) of
  Just _ -> cxt
  Nothing -> Map.insert str d cxt


checkProgram :: Program -> Result
checkProgram (Prog defs) =
  case defs of
    [] -> failure ""
    _ -> checkDefs defs (empty, empty)


checkDefs :: [Def] -> (Context Type, Context Iso) -> Result
checkDefs defs (tcxt, fcxt) = case defs of
  [] -> Ok "Success!"
  d:ds ->
    case res of
      Ok _ -> checkDefs ds (newTcxt, newFcxt)
      Bad x -> failure x
    where
      (newTcxt, newFcxt, res) = checkDef d (tcxt, fcxt)


checkDef :: Def -> (Context Type, Context Iso) -> (Context Type, Context Iso, Result)
checkDef def (tcxt, fcxt) = case def of
  DType (Ident str) ty -> case (checkType ty tcxt) of
    Ok o -> (update str ty tcxt, fcxt, Ok o)
    Bad x -> (tcxt, fcxt, Bad x)
  DFunc (Ident str) tyIn tyOut f -> case (checkFunc f (tyIn, tyOut) (tcxt, fcxt)) of
    Ok o -> (tcxt, update str (f, tyIn, tyOut) fcxt, Ok o)
    Bad x -> (tcxt, fcxt, Bad x)


checkType :: Type -> Context Type -> Result
checkType ty tcxt = case ty of
  TUnit -> Ok ""
  TTensor t1 t2 ->
    case (res1, res2) of
      (Ok _, Ok _) -> Ok ""
      _ -> failure ""
    where
      res1 = checkType t1 tcxt
      res2 = checkType t2 tcxt
  TSum t1 t2 ->
    case (res1, res2) of
      (Ok _, Ok _) -> Ok ""
      _ -> failure ""
    where
      res1 = checkType t1 tcxt
      res2 = checkType t2 tcxt
  TStar t -> checkType t tcxt
  TVar (Ident str) ->
    case (Map.lookup str tcxt) of
      Just _ -> Ok ""
      Nothing -> failure ""


checkFunc :: Func -> (Type, Type) -> (Context Type, Context Iso) -> Result
checkFunc func (tyIn, tyOut) (tcxt, fcxt) = case func of
  FId -> if tyIn == tyOut
         then Ok ""
         else failure ""
  
  FComp f1 f2 -> case maybeTy of
    Just _ -> Ok ""
    Nothing -> failure ""
    where
      maybeTy = searchOutType (FComp f1 f2) tyIn fcxt
  
  FTensor func1 func2 -> case (tyIn, tyOut) of
    (TTensor t1 t2, TTensor t1' t2') ->
      case (res1, res2) of
        (Ok _, Ok _) -> Ok ""
        _ -> failure ""
      where
        res1 = checkFunc func1 (t1, t1') (tcxt, fcxt)
        res2 = checkFunc func2 (t2, t2') (tcxt, fcxt)
    _ -> failure ""
  
  FTensUnit -> case (tyIn, tyOut) of
    (TTensor TUnit t, t') -> if t == t'
      then Ok ""
      else failure ""
    _ -> failure ""
  
  FTensAssoc -> case (tyIn, tyOut) of
    (TTensor t1 (TTensor t2 t3), TTensor (TTensor t1' t2') t3') ->
      if t1 == t1' && t2 == t2' && t3 == t3'
      then Ok ""
      else failure ""
    _ -> failure ""
  
  FTensSym -> case (tyIn, tyOut) of
    (TTensor t1 t2, TTensor t2' t1') ->
      if t1 == t1' && t2 == t2'
      then Ok ""
      else failure ""
    _ -> failure ""
  
  FSum func1 func2 -> case (tyIn, tyOut) of
    (TSum t1 t2, TSum t1' t2') ->
      case (res1, res2) of
        (Ok _, Ok _) -> Ok ""
        _ -> failure ""
      where
        res1 = checkFunc func1 (t1, t1') (tcxt, fcxt)
        res2 = checkFunc func2 (t2, t2') (tcxt, fcxt)
    _ -> failure ""
  
  FSumAssoc -> case (tyIn, tyOut) of
    (TSum t1 (TSum t2 t3), TSum (TSum t1' t2') t3') ->
      if t1 == t1' && t2 == t2' && t3 == t3'
      then Ok ""
      else failure ""
    _ -> failure ""
  
  FSumSym -> case (tyIn, tyOut) of
    (TSum t1 t2, TSum t2' t1') ->
      if t1 == t1' && t2 == t2'
      then Ok ""
      else failure ""
    _ -> failure ""
  
  FDistrib -> case (tyIn, tyOut) of
    (TTensor (TSum t1 t2) t3, TSum (TTensor t1' t3') (TTensor t2' t3'')) ->
      if t1 == t1' && t2 == t2' && t3 == t3' && t3' == t3''
      then Ok ""
      else failure ""
    _ -> failure ""
  
  FEval t -> case (tyIn, tyOut) of
    (TTensor t' (TStar t''), TUnit) ->
      if t == t' && t' == t''
      then Ok ""
      else failure ""
    _ -> failure ""
  
  FDagger f -> checkFunc f (tyOut, tyIn) (tcxt, fcxt)
  
  FVar (Ident str) ->
    case (Map.lookup str fcxt) of
      Just _ -> Ok ""
      Nothing -> failure ""

  FShift _ -> Ok ""


searchOutType :: Func -> Type -> Context Iso -> Maybe Type
searchOutType func inType cxt = case (func, inType) of
  (FId, t) ->
    Just t
  
  (FComp f1 f2, t1) ->
    case maybe2 of
      Just t2 -> searchOutType f2 t2 cxt
      Nothing -> Nothing
    where
      maybe2 = searchOutType f1 t1 cxt
  
  (FTensor f1 f2, TTensor t1 t2) ->
    case (maybe1, maybe2) of
      (Just t1', Just t2') -> Just (TTensor t1' t2')
      _ -> Nothing
    where
      maybe1 = (searchOutType f1 t1 cxt)
      maybe2 = (searchOutType f2 t2 cxt)
  
  (FTensUnit, TTensor TUnit t) ->
    Just t
  
  (FTensAssoc, TTensor t1 (TTensor t2 t3)) ->
    Just (TTensor (TTensor t1 t2) t3)
  
  (FTensSym, TTensor t1 t2) ->
    Just (TTensor t2 t1)
  
  (FSum f1 f2, TSum t1 t2) ->
    case (maybe1, maybe2) of
      (Just t1', Just t2') -> Just (TSum t1' t2')
      _ -> Nothing
    where
      maybe1 = (searchOutType f1 t1 cxt)
      maybe2 = (searchOutType f2 t2 cxt)
  
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
    searchInType f t cxt
  
  (FVar (Ident str), t) ->
    case (Map.lookup str cxt) of
      Just (_, tyIn, tyOut) ->
        if t == tyIn
        then Just tyOut
        else Nothing
      Nothing -> Nothing

  (FShift _, t) ->
    Just t
  
  _ -> Nothing

searchInType :: Func -> Type -> Context Iso -> Maybe Type
searchInType func outType cxt = case (func, outType) of
  (FId, t) ->
    Just t
  
  (FComp f1 f2, t3) ->
    case maybe2 of
      Just t2 -> searchInType f1 t2 cxt
      Nothing -> Nothing
    where
      maybe2 = searchInType f2 t3 cxt
  
  (FTensor f1 f2, TTensor t1 t2) ->
    case (maybe1, maybe2) of
      (Just t1', Just t2') -> Just (TTensor t1' t2')
      _ -> Nothing
    where
      maybe1 = (searchInType f1 t1 cxt)
      maybe2 = (searchInType f2 t2 cxt)
  
  (FTensUnit, t) ->
    Just (TTensor TUnit t)
  
  (FTensAssoc, TTensor (TTensor t1 t2) t3) ->
    Just (TTensor t1 (TTensor t2 t3))
    
  (FTensSym, TTensor t1 t2) ->
    Just (TTensor t2 t1)
  
  (FSum f1 f2, TSum t1 t2) ->
    case (maybe1, maybe2) of
      (Just t1', Just t2') -> Just (TSum t1' t2')
      _ -> Nothing
    where
      maybe1 = (searchInType f1 t1 cxt)
      maybe2 = (searchInType f2 t2 cxt)

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
    searchOutType f t cxt
  
  (FVar (Ident str), t) ->
    case (Map.lookup str cxt) of
      Just (_, tyIn, tyOut) ->
        if t == tyOut
        then Just tyIn
        else Nothing
      Nothing -> Nothing

  (FShift _, t) ->
    Just t
  
  _ -> Nothing
