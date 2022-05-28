module Utils where

import Omnirev.AbsOmnirev
import Omnirev.ErrM
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Prelude hiding ((++), (*), id, not, (&&), (**), (*>), (||))
import Control.Monad ( liftM, liftM2 )


tyvar :: String -> Type
tyvar x = TyVar $ Ident x
unit :: Type
unit = TyUnit
(++) :: Type -> Type -> Type
ty1 ++ ty2 = TySum ty1 ty2
(**) :: Type -> Type -> Type
ty1 ** ty2 = TyTensor ty1 ty2
(*>) :: Type -> Type -> Type
ty1 *> ty2 = TyFunc ty1 ty2
fix :: String -> Type -> Type
fix x = TyRec (Ident x)

tmvar :: String -> Term
tmvar x = TmVar $ Ident x
i :: Term
i = TmUnit
inl :: Term -> Term
inl = TmLeft
inr :: Term -> Term
inr = TmRight
(*) :: Term -> Term -> Term
tm1 * tm2 = TmTensor tm1 tm2
(+>) :: Term -> Term -> Term
tm1 +> tm2 = TmArrow tm1 tm2
fold :: Type -> Term -> Term
fold = TmFold
(||) :: Term -> Term -> Term
tm1 || tm2 = TmLin tm1 tm2
(&&) :: Term -> Term -> Term
tm1 && tm2 = TmLin tm1 tm2
trace :: Type -> Term -> Term
trace = TmTrace
rev :: Term -> Term
rev = TmFlip
em :: Term
em = TmEmpty
id :: Term
id = TmId

qubit :: Type
qubit = unit ++ unit
tens :: Type
tens = qubit ** qubit
circuit :: Type
circuit = qubit *> qubit
nat :: Type
nat = fix "x" (unit ++ tyvar "x")
natlist :: Type
natlist = fix "y" unit ++ (nat ** tyvar "y")

true :: Term
true = inl i
false :: Term
false = inr i
not :: Term
not = (true +> false) || (true +> false)
swap :: Term
swap = ((true  * true)  +> (true  * true))
    || ((true  * false) +> (false * true))
    || ((false * true)  +> (true  * false))
    || ((false * false) +> (false * false))
cnot :: Term
cnot = ((true  * true)  +> (true  * true))
    || ((true  * false) +> (true  * false))
    || ((false * true)  +> (false * false))
    || ((false * false) +> (false * true))
notandid :: Term
notandid = swap && id && rev swap
add :: Term
add = trace ((nat ** nat) ** nat)
  (  inr (tmvar "x" * tmvar "y")                                +> inl ((tmvar "y" * fold nat (inl i)) * tmvar "x")
  || inl ((tmvar "a" * tmvar "b") * fold nat (inr (tmvar "n"))) +> inl ((fold nat (inr (tmvar "a")) * fold nat (inr (tmvar "b"))) * tmvar "n")
  || inl ((tmvar "a" * tmvar "b") * fold nat (inl i))           +> inr (tmvar "a" * tmvar "b")
  )
one :: Term
one = fold nat $ inr $ fold nat $ inl i

program :: Program
program =
  Prog [ DType (Ident "qubit") qubit
       , DTerm (Ident "not") (qubit *> qubit) not
       , DTerm (Ident "true") qubit true
       , DTerm (Ident "false") qubit false
       , DExpr (Ident "test") qubit $ ExApp (ExTerm not) true
       , DTerm (Ident "swap") ((qubit ** qubit) *> (qubit ** qubit)) swap
       , DTerm (Ident "cnot") ((qubit ** qubit) *> (qubit ** qubit)) cnot
       , DExpr (Ident "quant") (qubit ** qubit) $ ExApp (ExTerm swap) (true * false)
       , DTerm (Ident "notandid") ((qubit ** qubit) *> (qubit ** qubit)) notandid
       , DTerm (Ident "hoge") (qubit ** qubit) (true * false)
       , DExpr (Ident "same") (qubit ** qubit) $ ExApp (ExTerm notandid) $ tmvar "hoge"
       , DType (Ident "nat") nat
       , DTerm (Ident "add") ((nat ** nat) *> (nat ** nat)) add
       , DTerm (Ident "one") nat one
       , DExpr (Ident "two") (nat ** nat) $ ExApp (ExTerm add) (one * one)
       ]

program' :: Program
program' =
  Prog [ DType (Ident "qubit") (TySum TyUnit TyUnit)
       , DTerm (Ident "not") (TyFunc (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TmLin (TmArrow (TmLeft TmUnit) (TmRight TmUnit)) (TmArrow (TmRight TmUnit) (TmLeft TmUnit)))
       , DTerm (Ident "true") (TyVar (Ident "qubit")) (TmLeft TmUnit)
       , DTerm (Ident "false") (TyVar (Ident "qubit")) (TmRight TmUnit)
       , DExpr (Ident "test") (TyVar (Ident "qubit")) (ExApp (ExTerm (TmVar (Ident "not"))) (TmVar (Ident "true")))
       , DTerm (Ident "swap") (TyFunc (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))) (TmLin (TmLin (TmLin (TmArrow (TmTensor (TmLeft TmUnit) (TmLeft TmUnit)) (TmTensor (TmLeft TmUnit) (TmLeft TmUnit))) (TmArrow (TmTensor (TmLeft TmUnit) (TmRight TmUnit)) (TmTensor (TmRight TmUnit) (TmLeft TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmLeft TmUnit)) (TmTensor (TmLeft TmUnit) (TmRight TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmRight TmUnit)) (TmTensor (TmRight TmUnit) (TmRight TmUnit))))
       , DTerm (Ident "cnot") (TyFunc (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))) (TmLin (TmLin (TmLin (TmArrow (TmTensor (TmLeft TmUnit) (TmLeft TmUnit)) (TmTensor (TmLeft TmUnit) (TmLeft TmUnit))) (TmArrow (TmTensor (TmLeft TmUnit) (TmRight TmUnit)) (TmTensor (TmLeft TmUnit) (TmRight TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmLeft TmUnit)) (TmTensor (TmRight TmUnit) (TmRight TmUnit)))) (TmArrow (TmTensor (TmRight TmUnit) (TmRight TmUnit)) (TmTensor (TmRight TmUnit) (TmLeft TmUnit))))
       , DExpr (Ident "quant") (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (ExApp (ExTerm (TmVar (Ident "swap"))) (TmTensor (TmVar (Ident "true")) (TmVar (Ident "false"))))
       , DTerm (Ident "notandid") (TyFunc (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit")))) (TmComp (TmComp (TmVar (Ident "swap")) TmId) (TmFlip (TmVar (Ident "swap"))))
       , DTerm (Ident "hoge") (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (TmTensor (TmVar (Ident "true")) (TmVar (Ident "false")))
       , DExpr (Ident "same") (TyTensor (TyVar (Ident "qubit")) (TyVar (Ident "qubit"))) (ExApp (ExTerm (TmVar (Ident "notandid"))) (TmVar (Ident "hoge")))
       , DType (Ident "nat") (TyRec (Ident "X") (TySum TyUnit (TyVar (Ident "X"))))
       , DTerm (Ident "add") (TyFunc (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat")))) (TmTrace (TyTensor (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (TyVar (Ident "nat"))) (TmLin (TmLin (TmArrow (TmRight (TmTensor (TmVar (Ident "x")) (TmVar (Ident "y")))) (TmLeft (TmTensor (TmTensor (TmVar (Ident "y")) (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit))) (TmVar (Ident "x"))))) (TmArrow (TmLeft (TmTensor (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))) (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "n")))))) (TmLeft (TmTensor (TmTensor (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "a")))) (TmFold (TyVar (Ident "nat")) (TmRight (TmVar (Ident "b"))))) (TmVar (Ident "n")))))) (TmArrow (TmLeft (TmTensor (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b"))) (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit)))) (TmRight (TmTensor (TmVar (Ident "a")) (TmVar (Ident "b")))))))
       , DTerm (Ident "one") (TyVar (Ident "nat")) (TmFold (TyVar (Ident "nat")) (TmRight (TmFold (TyVar (Ident "nat")) (TmLeft TmUnit))))
       , DExpr (Ident "two") (TyTensor (TyVar (Ident "nat")) (TyVar (Ident "nat"))) (ExApp (ExTerm (TmVar (Ident "add"))) (TmTensor (TmVar (Ident "one")) (TmVar (Ident "one"))))
       ]


instance Arbitrary Ident where
  arbitrary = oneof [Ident <$> arbitrary]

arbType :: Int -> Gen Type
arbType 0 = oneof [TyVar <$> arbitrary, pure TyUnit]
arbType n = do
  let n' = n `div` 2
  let m' = n - n'
  oneof [ TyVar <$> arbitrary
        , pure TyUnit
        , liftM2 TySum (arbType n') (arbType m')
        , liftM2 TyTensor (arbType n') (arbType m')
        , liftM2 TyFunc (arbType n') (arbType m')
        , liftM2 TyRec arbitrary (arbType $ n-1)
        ]

instance Arbitrary Type where
  arbitrary = sized arbType

arbTerm :: Int -> Gen Term
arbTerm 0 = oneof [TmVar <$> arbitrary, pure TmUnit, pure TmEmpty, pure TmId]
arbTerm n = do
  let n' = n `div` 2
  let m' = n - n'
  oneof [ TmVar <$> arbitrary
        , pure TmUnit
        , fmap TmLeft (arbTerm $ n-1)
        , fmap TmRight (arbTerm $ n-1)
        , liftM2 TmTensor (arbTerm n') (arbTerm m')
        , liftM2 TmArrow (arbTerm n') (arbTerm m')
        , liftM2 TmFold (arbType n') (arbTerm m')
        , liftM2 TmLin (arbTerm n') (arbTerm m')
        , liftM2 TmTrace (arbType n') (arbTerm m')
        , liftM2 TmComp (arbTerm n') (arbTerm m')
        , fmap TmFlip (arbTerm $ n-1)
        , pure TmEmpty
        , pure TmId
        ]

instance Arbitrary Term where
  arbitrary = sized arbTerm