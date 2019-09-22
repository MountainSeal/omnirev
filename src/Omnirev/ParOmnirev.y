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
  '!' { PT _ (TS _ 1) }
  '(' { PT _ (TS _ 2) }
  '(*)' { PT _ (TS _ 3) }
  '(+)' { PT _ (TS _ 4) }
  ')' { PT _ (TS _ 5) }
  '*' { PT _ (TS _ 6) }
  '+' { PT _ (TS _ 7) }
  ',' { PT _ (TS _ 8) }
  '.' { PT _ (TS _ 9) }
  ':' { PT _ (TS _ 10) }
  ';' { PT _ (TS _ 11) }
  '=' { PT _ (TS _ 12) }
  'I' { PT _ (TS _ 13) }
  'be' { PT _ (TS _ 14) }
  'case' { PT _ (TS _ 15) }
  'cis' { PT _ (TS _ 16) }
  'fix' { PT _ (TS _ 17) }
  'fold' { PT _ (TS _ 18) }
  'fst' { PT _ (TS _ 19) }
  'in' { PT _ (TS _ 20) }
  'inl' { PT _ (TS _ 21) }
  'inr' { PT _ (TS _ 22) }
  'let' { PT _ (TS _ 23) }
  'measure' { PT _ (TS _ 24) }
  'of' { PT _ (TS _ 25) }
  'pi' { PT _ (TS _ 26) }
  'rec' { PT _ (TS _ 27) }
  'skip' { PT _ (TS _ 28) }
  'snd' { PT _ (TS _ 29) }
  'term' { PT _ (TS _ 30) }
  'to' { PT _ (TS _ 31) }
  'type' { PT _ (TS _ 32) }
  '~' { PT _ (TS _ 33) }

L_ident  { PT _ (TV $$) }
L_doubl  { PT _ (TD $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }

Program :: { Program }
Program : ListDef { AbsOmnirev.Prog $1 }
Def :: { Def }
Def : 'type' Ident '=' Type { AbsOmnirev.DType $2 $4 }
    | 'term' Ident ':' Type '=' Term { AbsOmnirev.DTerm $2 $4 $6 }
ListDef :: { [Def] }
ListDef : Def { (:[]) $1 } | Def ListDef { (:) $1 $2 }
Value6 :: { Value }
Value6 : 'cis' Double 'pi' { AbsOmnirev.VUnit $2 }
       | 'inl' Value6 { AbsOmnirev.VLeft $2 }
       | 'inr' Value6 { AbsOmnirev.VRight $2 }
       | '~' Value6 { AbsOmnirev.VDual $2 }
       | 'fold' Value6 { AbsOmnirev.VFold $2 }
Value5 :: { Value }
Value5 : Value5 '*' Value6 { AbsOmnirev.VTensor $1 $3 }
       | '(' Value ')' { $2 }
Value1 :: { Value }
Value1 : Value1 ',' Value2 { AbsOmnirev.VTrace $1 $3 }
       | Value2 { $1 }
Value2 :: { Value }
Value2 : Value2 Value3 { AbsOmnirev.VApp $1 $2 } | Value3 { $1 }
Value3 :: { Value }
Value3 : Value3 ';' Value4 { AbsOmnirev.VComp $1 $3 }
       | Value4 { $1 }
Value4 :: { Value }
Value4 : Value4 '+' Value5 { AbsOmnirev.VSum $1 $3 }
       | Value5 { $1 }
Value :: { Value }
Value : Value1 { $1 }
Term :: { Term }
Term : Ident { AbsOmnirev.CVar $1 }
     | Value { AbsOmnirev.CValue $1 }
     | '!' Value { AbsOmnirev.CBang $2 }
     | 'measure' Value { AbsOmnirev.CMeas $2 }
     | 'skip' { AbsOmnirev.CSkip }
     | 'case' Term 'of' 'inl' Ident 'to' Term 'inr' Ident 'to' Term { AbsOmnirev.CCase $2 $5 $7 $9 $11 }
     | 'fst' Term { AbsOmnirev.CFst $2 }
     | 'snd' Term { AbsOmnirev.CSnd $2 }
     | 'let' Ident 'be' Term 'in' Term { AbsOmnirev.CLet $2 $4 $6 }
     | 'rec' Ident 'be' Term { AbsOmnirev.CRec $2 $4 }
Type3 :: { Type }
Type3 : 'I' { AbsOmnirev.TUnit }
      | '~' Type3 { AbsOmnirev.TDual $2 }
      | 'fix' Ident '.' Type { AbsOmnirev.TInd $2 $4 }
      | Ident { AbsOmnirev.TVar $1 }
      | '(' Type ')' { $2 }
Type1 :: { Type }
Type1 : Type1 '(+)' Type2 { AbsOmnirev.TSum $1 $3 } | Type2 { $1 }
Type2 :: { Type }
Type2 : Type2 '(*)' Type3 { AbsOmnirev.TTensor $1 $3 }
      | Type3 { $1 }
Type :: { Type }
Type : Type1 { $1 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}

