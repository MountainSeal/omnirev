-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Omnirev.ParOmnirev where
import Omnirev.AbsOmnirev as AbsOmnirev
import Omnirev.LexOmnirev
import Omnirev.ErrM

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  '*' { PT _ (TS _ 3) }
  '+' { PT _ (TS _ 4) }
  ',' { PT _ (TS _ 5) }
  '->' { PT _ (TS _ 6) }
  '.' { PT _ (TS _ 7) }
  ':' { PT _ (TS _ 8) }
  ';' { PT _ (TS _ 9) }
  '=' { PT _ (TS _ 10) }
  '=>' { PT _ (TS _ 11) }
  'I' { PT _ (TS _ 12) }
  'expr' { PT _ (TS _ 13) }
  'fix' { PT _ (TS _ 14) }
  'fold' { PT _ (TS _ 15) }
  'inl' { PT _ (TS _ 16) }
  'inr' { PT _ (TS _ 17) }
  'type' { PT _ (TS _ 18) }
  'unit' { PT _ (TS _ 19) }
  'where' { PT _ (TS _ 20) }
  '|' { PT _ (TS _ 21) }
  '~' { PT _ (TS _ 22) }
  L_ident  { PT _ (TV $$) }

%%

Ident   :: { Ident }
Ident    : L_ident  { Ident $1 }

Program :: { Program }
Program : ListDef { AbsOmnirev.Prog $1 }
Def :: { Def }
Def : 'type' Ident '=' Type { AbsOmnirev.DType $2 $4 }
    | 'expr' Ident ':' Type '=' Expr { AbsOmnirev.DTerm $2 $4 $6 }
ListDef :: { [Def] }
ListDef : Def { (:[]) $1 } | Def ListDef { (:) $1 $2 }
Type4 :: { Type }
Type4 : Ident { AbsOmnirev.TyVar $1 }
      | 'I' { AbsOmnirev.TyUnit }
      | 'fix' Ident '.' Type4 { AbsOmnirev.TyRec $2 $4 }
      | '(' Type ')' { $2 }
Type2 :: { Type }
Type2 : Type2 '+' Type3 { AbsOmnirev.TySum $1 $3 } | Type3 { $1 }
Type3 :: { Type }
Type3 : Type3 '*' Type4 { AbsOmnirev.TyTensor $1 $3 }
      | Type4 { $1 }
Type1 :: { Type }
Type1 : Type1 '->' Type2 { AbsOmnirev.TyFunc $1 $3 } | Type2 { $1 }
Type :: { Type }
Type : Type1 { $1 }
Term4 :: { Term }
Term4 : Ident { AbsOmnirev.TmVar $1 }
      | 'unit' { AbsOmnirev.TmUnit }
      | 'inl' Term4 { AbsOmnirev.TmLeft $2 }
      | 'inr' Term4 { AbsOmnirev.TmRight $2 }
      | 'fold' Type Term4 { AbsOmnirev.TmFold $2 $3 }
      | Ident Term4 { AbsOmnirev.TmLabel $1 $2 }
      | '(' Term ')' { $2 }
Term3 :: { Term }
Term3 : Term3 ',' Term4 { AbsOmnirev.TmTensor $1 $3 }
      | Term4 { $1 }
Term2 :: { Term }
Term2 : Term2 '=>' Term3 { AbsOmnirev.TmArrow $1 $3 }
      | Term3 { $1 }
Term1 :: { Term }
Term1 : Term1 '|' Term2 { AbsOmnirev.TmLin $1 $3 } | Term2 { $1 }
Term :: { Term }
Term : Term1 { $1 }
Expr2 :: { Expr }
Expr2 : Term { AbsOmnirev.ExTerm $1 }
      | Expr2 Term { AbsOmnirev.ExApp $1 $2 }
      | '~' Expr2 { AbsOmnirev.ExFlip $2 }
      | Term 'where' Ident ':' Type { AbsOmnirev.ExTrace $1 $3 $5 }
      | '(' Expr ')' { $2 }
Expr1 :: { Expr }
Expr1 : Expr1 ';' Expr2 { AbsOmnirev.ExComp $1 $3 } | Expr2 { $1 }
Expr :: { Expr }
Expr : Expr1 { $1 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}

