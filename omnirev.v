Require Import Arith.
Require Import Peano.
Require Import PeanoNat.
Require Import Basics.
Require Import Bool.
Require Import String.
Require Import List.
Import ListNotations.
Require Import ListSet.
Require Import MSets.MSetWeakList.
Require Import FMapList.
Require Import Program.Wf.
Require Import Recdef.
Require Import Structures.Equalities.
Require Import Structures.OrderedType.
(* Require Import Structures.Orders. *)


Set Implicit Arguments.
Set Contextual Implicit.


Module Omnirev.
Module ASTree.
Inductive id : Type :=
| Id : nat -> id
.

Scheme Equality for id.

Module IdEq <: DecidableType.
Definition t := id.
Definition eq := @eq id.
Definition eq_equiv : Equivalence eq := _.
Definition eq_dec := @id_eq_dec.
Include BackportEq.
End IdEq.

Module IdSet := MSetWeakList.Make IdEq.

Module IdOrd <: OrderedType.
Include IdEq.

Definition id_lt i1 i2 : Prop :=
  match (i1, i2) with
  | (Id n1, Id n2) => lt n1 n2
  end%signature.

Definition lt := id_lt.

Theorem lt_id_nat : forall n m,
    n < m -> lt (Id n) (Id m).
Proof.
  intros. apply H.
Qed.

Theorem lt_nat_id : forall n m,
    lt (Id n) (Id m) -> n < m.
Proof.
  intros. apply H.
Qed.

Lemma lt_trans : Transitive lt.
Proof.
  unfold Transitive. intros x y z. destruct x, y, z.
  intros. apply lt_id_nat. apply lt_nat_id in H. apply lt_nat_id in H0.
  apply Nat.lt_trans with ( m := n0).
  apply H. apply H0.
Qed.

Lemma lt_not_eq : forall x y : t, lt x y -> ~ eq x y.
Proof.
  unfold not. intros. destruct x,y. rename n0 into m. inversion H0. apply lt_nat_id in H. rewrite H2 in H. apply Nat.lt_irrefl in H. apply H.
Qed.

Lemma lt_irrefl : Irreflexive lt.
Proof.
  unfold Irreflexive.  unfold Reflexive. unfold complement.
  intros. destruct x. apply lt_nat_id in H. apply Nat.lt_irrefl in H.
  inversion H.
Qed.

#[local] Instance lt_strorder : StrictOrder lt.
Proof.
  split; [exact lt_irrefl | exact lt_trans].
Qed.

#[local] Instance lt_compat : Proper (eq==>eq==>iff) lt := _.
(* Proof. *)
  (* split; intros; destruct x,y,x0,y0; apply lt_nat_id in H1; apply lt_id_nat. *)
  (* unfold eq in H. *)

Definition id_compare (x y : id) : comparison :=
  match (x, y) with
  | (Id n1, Id n2) =>  n1 ?= n2
  end.

Definition cmp := id_compare.

Lemma compare_eq : forall x y,
    cmp x y = Eq -> x = y.
Proof.
  intros. destruct x,y. rename n0 into m. f_equal.
  unfold cmp in H. apply Nat.compare_eq_iff in H. apply H.
Qed.

Lemma compare_lt : forall x y,
    cmp x y = Lt -> lt x y.
Proof.
  intros. destruct x,y. rename n0 into m. apply lt_id_nat.
  unfold cmp in H. apply Nat.compare_lt_iff in H. apply H.
Qed.

Lemma compare_gt : forall x y,
    cmp x y = Gt -> lt y x.
Proof.
  intros. destruct x,y. rename n0 into m. apply lt_id_nat.
  unfold cmp in H. apply Nat.compare_gt_iff in H. apply H.
Qed.

Lemma compare_spec : forall x y, CompSpec eq lt x y (cmp x y).
Proof.
  intros.
  unfold CompSpec.
  case_eq (cmp x y); constructor.
  apply compare_eq. exact H.
  apply compare_lt. exact H.
  apply compare_gt. exact H.
Qed.

Local Lemma compare_helper_lt {x y : id} (L : cmp x y = Lt):
  lt x y.
Proof.
  now apply compare_lt.
Qed.

Local Lemma compare_helper_gt {x y : id} (G : cmp x y = Gt):
  lt y x.
Proof.
  now apply compare_gt.
Qed.

Local Lemma compare_helper_eq {x y : id} (E : cmp x y = Eq):
  x = y.
Proof.
  now apply compare_eq.
Qed.

(* Definition compare := cmp. *)

(* for FMapList *)
Definition compare (x y : id) : Compare lt eq x y :=
  match (cmp x y) as z return _ = z -> _ with
  | Lt => fun E => LT (compare_helper_lt E)
  | Eq => fun E => EQ (compare_helper_eq E)
  | Gt => fun E => GT (compare_helper_gt E)
  end Logic.eq_refl.

End IdOrd.

Module IdMap := FMapList.Make IdOrd.

Definition beq_id id1 id2 :=
  match (id1, id2) with
    (Id n1, Id n2) => beq_nat n1 n2
  end.

Theorem beq_id_refl : forall i,
    true = beq_id i i.
Proof.
  intros. destruct i.
  apply beq_nat_refl.
Qed.

Theorem beq_id_eq : forall i1 i2,
    true = beq_id i1 i2 -> i1 = i2.
Proof.
  intros i1 i2 H.
  destruct i1. destruct i2.
  apply beq_nat_eq in H. subst.
  reflexivity.
Qed.

Theorem beq_id_false_not_eq : forall i1 i2,
    beq_id i1 i2 = false -> i1 <> i2.
Proof.
  intros i1 i2 H.
  destruct i1. destruct i2.
  apply beq_nat_false in H.
  intros C. apply H. inversion C. reflexivity.
Qed.

Theorem not_eq_beq_false : forall n n' : nat,
    n <> n' ->
    beq_nat n n' = false.
Proof.
  induction n; destruct n'; simpl; intros; try reflexivity.
  induction H; reflexivity.
  apply IHn.
  intro H'. rewrite H' in *. apply H.
  reflexivity.
Qed.

Theorem not_eq_beq_id_false : forall i1 i2,
    i1 <> i2 -> beq_id i1 i2 = false.
Proof.
  intros i1 i2 H.
  destruct i1. destruct i2.
  assert (n <> n0).
    intros C. subst. apply H. reflexivity.
  apply not_eq_beq_false. assumption.
Qed.

Theorem beq_nat_sym : forall n m,
    beq_nat n m = beq_nat m n.
Proof.
  intros n. induction n; destruct m; try reflexivity.
  apply IHn.
Qed.       

Theorem beq_id_sym : forall i1 i2,
    beq_id i1 i2 = beq_id i2 i1.
Proof.
  intros i1 i2. destruct i1. destruct i2. apply beq_nat_sym.
Qed.

Definition partial_map (A:Type) := id -> option A.

Definition empty {A:Type} : partial_map A := (fun _ => None).

Definition extend {A:Type} (Gamma : partial_map A) (x:id) (T:A) :=
  fun x' => if beq_id x x' then Some T else Gamma x'.

Notation "'∅'" := empty.
Notation "Γ '，' x '：' T" := (extend Γ x T) (at level 80).

Lemma extend_eq : forall A (ctxt : partial_map A) x T,
    (ctxt ， x：T) x = Some T.
Proof.
  intros. unfold extend. rewrite <- beq_id_refl. reflexivity.
Qed.

Lemma extend_neq : forall A (ctxt : partial_map A) x1 T x2,
    beq_id x2 x1 = false ->
    (ctxt ， x2：T) x1 = ctxt x1.
Proof.
  intros. unfold extend. rewrite H. reflexivity.
Qed.

Lemma extend_shadow : forall A (ctxt : partial_map A) t1 t2 x1 x2,
    ((ctxt ， x2：t1) ， x2：t2) x1 = (ctxt ， x2：t2) x1.
Proof.
  intros. unfold extend. destruct (beq_id x2 x1); reflexivity.
Qed.


Inductive ty : Type :=
| t_var    : id -> ty
| t_unit   : ty
| t_sum    : ty -> ty -> ty
| t_tensor : ty -> ty -> ty
| t_func   : ty -> ty -> ty
| t_rec    : id -> ty -> ty
.
Notation "'ﾋ' X" := (t_var X) (at level 10).
Notation "'I'" := t_unit.
Notation "A '⊕' B" := (t_sum A B) (at level 50).
Notation "A '⊗' B" := (t_tensor A B) (at level 50).
Notation "A '⊸' B" := (t_func A B) (at level 50).
Notation "X '∾' T" := (t_rec X T) (at level 50).

Fixpoint beq_ty (Ty Uy : ty) : bool :=
  match (Ty, Uy) with
  | (ﾋ X,  ﾋ Y) => beq_id X Y
  | (I, I) => true
  | (T1⊕T2, U1⊕U2) => (beq_ty T1 U1) && (beq_ty T2 U2)
  | (T1⊗T2, U1⊗U2) => (beq_ty T1 U1) && (beq_ty T2 U2)
  | (T1⊸T2, U1⊸U2) => (beq_ty T1 U1) && (beq_ty T2 U2)
  | (X∾T, Y∾U) => (beq_id X Y) && (beq_ty T U)
  | _ => false
  end.

Scheme Equality for ty.
Module TyEq : DecidableType with Definition t := ty.
Definition t := ty.
Definition eq := @eq ty.
Definition eq_equiv : Equivalence eq := _.
Definition eq_dec := @ty_eq_dec.
End TyEq.
Module TySet := MSetWeakList.Make TyEq.

Inductive tm : Type :=
| tm_var    : id -> tm
| tm_unit   : tm
| tm_left   : tm -> tm
| tm_right  : tm -> tm
| tm_tensor : tm -> tm -> tm
| tm_arrow  : tm -> tm -> tm
| tm_fold   : ty -> tm -> tm
| tm_lin    : tm -> tm -> tm
| tm_trace  : ty -> tm -> tm
| tm_comp   : tm -> tm -> tm
| tm_flip   : tm -> tm
| tm_nix    : tm
| tm_id     : tm
.
Notation "'ﾍ' x" := (tm_var x) (at level 10).
Notation "'unit'" := tm_unit.
Notation "'inl' t1" := (tm_left t1) (at level 10).
Notation "'inr' t2" := (tm_right t2) (at level 10).
Notation "t1 '×' t2" := (tm_tensor t1 t2) (at level 50).
Notation "t1 '↦' t2" := (tm_arrow t1 t2) (at level 50).
Notation "'fld' '[' T ']' t" := (tm_fold T t) (at level 50).
Notation "t1 '∥' t2" := (tm_lin t1 t2) (at level 50).
Notation "'trace' '[' T ']' t" := (tm_trace T t) (at level 50).
Notation "t1 '⨾' t2" := (tm_comp t1 t2) (at level 50).
Notation "'†' t" := (tm_flip t) (at level 10).
Notation "'nix'" := tm_nix.
Notation "'identity'" := tm_id.

Fixpoint beq_tm (tm um : tm) : bool :=
  match (tm, um) with
  | (ﾍ x, ﾍ y) => beq_id x y
  | (unit, unit) => true
  | (inl t, inl u) => beq_tm t u
  | (inr t, inr u) => beq_tm t u
  | (t1 × t2, u1 × u2) => (beq_tm t1 u1) && (beq_tm t2 u2)
  | (t1 ↦ t2, u1 ↦ u2) => (beq_tm t1 u1) && (beq_tm t2 u2)
  | (fld [T] t, fld [U] u) => (beq_ty T U) && (beq_tm t u)
  | (t1 ∥ t2, u1 ∥ u2) => (beq_tm t1 u1) && (beq_tm t2 u2)
  | (trace [T] t, trace [U] u) => (beq_ty T U) && (beq_tm t u)
  | (t1 ⨾ t2, u1 ⨾ u2) => (beq_tm t1 u1) && (beq_tm t2 u2)
  | († t, † u) => beq_tm t u
  | (nix, nix) => true
  | (identity, identity) => true
  | _ => false
  end.

Scheme Equality for tm.
Module TmEq : DecidableType with Definition t := tm.
Definition t := tm.
Definition eq := @eq tm.
Definition eq_equiv : Equivalence eq := _.
Definition eq_dec := @tm_eq_dec.
End TmEq.
Module TmSet := MSetWeakList.Make TmEq.

Fixpoint tm_size (trm : tm) : nat :=
  match trm with
  | ﾍ x => 1
  | unit => 1
  | inl t => S (tm_size t)
  | inr t => S (tm_size t)
  | t1 × t2 => S ((tm_size t1) + (tm_size t2))
  | t1 ↦ t2 => S ((tm_size t1) + (tm_size t2))
  | fld[T] t => S (tm_size t)
  | trace[T] t => S (tm_size t)
  | t1 ∥ t2 => S ((tm_size t1) + (tm_size t2))
  | t1 ⨾ t2 => S ((tm_size t1) + (tm_size t2))
  | † t => S (tm_size t)
  | identity => 1
  | nix => 1
  end.

Inductive ex : Type :=
| ex_tm    : tm -> ex
| ex_app   : ex -> tm -> ex
.
Notation "'ｼ' t" := (ex_tm t) (at level 50).
Notation "e '@' t" := (ex_app e t) (at level 60).

Scheme Equality for ex.
Module ExEq : DecidableType with Definition t := ex.
Definition t := ex.
Definition eq := @eq ex.
Definition eq_equiv : Equivalence eq := _.
Definition eq_dec := @ex_eq_dec.
End ExEq.
Module ExSet := MSetWeakList.Make ExEq.

End ASTree.


Module TyJudge.
Import ASTree.

Reserved Notation "Γ '|-' T" (at level 80).
Inductive form_type : partial_map ty -> ty -> Prop :=
| ft_var : forall Γ X T,
    Γ X = Some T ->
    (*----------------------*)
    Γ |- ﾋ X
| ft_unit : forall Γ,
    (*----------------------*)
    Γ |- I
| ft_sum : forall Γ T1 T2,
    Γ |- T1 -> Γ |- T2 ->
    (*----------------------*)
    Γ |- T1 ⊕ T2
| ft_tensor : forall Γ T1 T2,
    Γ |- T1 -> Γ |- T2 ->
    (*----------------------*)
    Γ |- T1 ⊗ T2
| ft_func : forall Γ T1 T2,
    Γ |- T1 -> Γ |- T2 ->
    (*----------------------*)
    Γ |- T1 ⊸ T2
| ft_rec : forall Γ X T,
    Γ，X：T |- T ->
    (*----------------------*)
    Γ |- X ∾ T
where "Γ '|-' T" := (form_type Γ T).

(*Haskell実装や言語の定義とは並びが逆順になっているに注意*)
(* Reserved Notation "'❲' S '⥷' X '❳' T" (at level 80). *)
Fixpoint ty_sbst (S:ty) (X:id) (T:ty) : ty :=
  match T with
  | ﾋ Y     => if beq_id X Y then S else ﾋ Y
  | I       => I
  | T1 ⊕ T2 => (ty_sbst S X T1) ⊕ (ty_sbst S X T2)
  | T1 ⊗ T2 => (ty_sbst S X T1) ⊗ (ty_sbst S X T2)
  | T1 ⊸ T2 => (ty_sbst S X T1) ⊸ (ty_sbst S X T2)
  | Y ∾ T'   => Y ∾ (ty_sbst S X T')
  end.
(* where "'❲' S '⥷' X '❳' T" := (ty_sbst S X T). *)
Notation "X '~>' S" := (ty_sbst S X) (at level 80).

Import IdSet.

Fixpoint ty_vars (T:ty) : IdSet.t :=
  match T with
  | ﾋ X     => singleton X
  | I       => empty
  | T1 ⊕ T2 => union (ty_vars T1) (ty_vars T2)
  | T1 ⊗ T2 => union (ty_vars T1) (ty_vars T2)
  | T1 ⊸ T2 => union (ty_vars T1) (ty_vars T2)
  | X ∾ T   => add X (ty_vars T)
  end.

Fixpoint ty_size (typ : ty) : nat :=
  match typ with
  | ﾋ X => 1
  | I => 1
  | T1 ⊕ T2 => S ((ty_size T1) + (ty_size T2))
  | T1 ⊗ T2 => S ((ty_size T1) + (ty_size T2))
  | T1 ⊸ T2 => S ((ty_size T1) + (ty_size T2))
  | X ∾ T => S (ty_size T)
  end.

Check In.

Definition occurs (X : id) (T : ty) : {In X (ty_vars T)}+{~In X (ty_vars T)}.
Proof.
  case_eq (mem X (ty_vars T)).
  left. rewrite <- mem_spec. rewrite H. reflexivity.
  right. rewrite <- mem_spec. unfold not. intros. rewrite H0 in H. inversion H.
Defined.

Fixpoint ty_equiv (T:ty) (U:ty) : bool :=
  match (T, U) with
  | (ﾋ X, ﾋ Y) => if beq_id X Y then true else false
  | (I, I) => true
  | (T1 ⊕ T2, U1 ⊕ U2) => (ty_equiv T1 U1) && (ty_equiv T2 U2)
  | (T1 ⊗ T2, U1 ⊗ U2) => (ty_equiv T1 U1) && (ty_equiv T2 U2)
  | (T1 ⊸ T2, U1 ⊸ U2) => (ty_equiv T1 U1) && (ty_equiv T2 U2)
  | (X ∾ T', Y ∾ U') =>
    if beq_id X Y
    then ty_equiv T' U'
    else false
      (* let v := unique_var T' U' in *)
      (* let T'' := ty_sbst (ﾋ v) X T' in *)
      (* let U'' := ty_sbst (ﾋ v) Y U' in *)
      (* ty_equiv T'' U'' *)
  | _ =>  false
  end.

Fixpoint ty_fv (T:ty) : IdSet.t :=
  match T with
  | ﾋ X     => singleton X
  | I       => empty
  | T1 ⊕ T2 => union (ty_fv T1) (ty_fv T2)
  | T1 ⊗ T2 => union (ty_fv T1) (ty_fv T2)
  | T1 ⊸ T2 => union (ty_fv T1) (ty_fv T2)
  | X ∾ T => remove X (ty_fv T)
  end.

Notation "s1 '∪' s2" := (union s1 s2) (at level 70).
Notation "s1 '∩' s2" := (inter s1 s2) (at level 70).
Notation "s1 '\' s2" := (diff s1 s2) (at level 70).
Notation "x '∈' s" := (mem x s = true) (at level 70).
Notation "x '∉' s" := (mem x s <> true) (at level 70).
Notation "'❲' x '❳'" := (singleton x).
Notation "'❲❳'" := empty.

Definition decapsulate := fun i => let '(Id n) := i in n.
Definition sum (iset:IdSet.t) : nat := fold (fun e n => (decapsulate e) + n) iset 0.
Definition uvar (iset:IdSet.t) : id :=
  if is_empty iset
  then Id 0
  else let n := list_max (map decapsulate (elements iset)) in (Id n).
Definition unique_var (T1:ty) (T2:ty) : id :=
  let vs := (ty_vars T1) ∪ (ty_vars T2) in Id ((sum vs) + 1).

Definition typed : Type := tm * ty.

Definition env := list typed.

Notation "t '⦂' T" := (t, T) (at level 80).
Notation "Γ '#' c" := (c :: Γ) (at level 80).
Notation "Γ '⊨' c" := (Γ, c) (at level 80).

Definition cstrs := list (ty * ty).
Notation "T1 '≖' T2" := (T1, T2) (at level 80).

(* 同じ変数名の変数を、最初に見つかったものを除外する *)
Fixpoint delete (x:typed) (l:env) : env :=
  match l with
  | []      => []
  | (y::l') =>
      match (x,y) with
      | ((t1, T1), (t2, T2)) =>
          if beq_tm t1 t2
          then l
          else y :: (delete x l')
      end
  end.

(* 最初に見つかったものを返す  *)
Fixpoint lookup (x : id) (env : list typed) : option ty :=
  match env with
  | [] => None
  | (typ::env') =>
      match typ with
      | (t ⦂ T) =>
          if beq_tm (ﾍ x) t
          then Some T
          else lookup x env'
      end
  end.

(* isetは変数集合を最初に与えるため *)
(* Fixpoint infer (e:env) (trm:tm) (typ:ty) (iset:IdSet.t) {struct trm} : (env * IdSet.t * cstrs) :=
  match (e, (trm, typ)) with
  (* unit_l *)
  | Γ # (unit ⦂ V) ⊨ (t ⦂ T) =>
      let '(Γ', χ, C) := infer Γ t T iset in (Γ', χ, C#(V≖T))
  (* unit_r *)
  | Γ ⊨ (unit ⦂ V) =>
      (Γ, ❲❳, [V≖I])
  (* inl_l *)
  | Γ # (inl t1 ⦂ V) ⊨ (t ⦂ T) =>
      let X1 := uvar iset in
      let X2 := uvar (iset∪❲X1❳) in
      let '(Γ', χ, C) := infer (Γ#(t1 ⦂ ﾋ X1)) t T iset in
      (Γ', χ∪❲X1❳∪❲X2❳, C # (V ≖ ﾋ X1 ⊕ ﾋ X2))
  (* otherwise *)
  | _ => ([], empty, [])
  end. *)

Reserved Notation "Γ '⊨' c '▷' Θ '⃒' χ '⟪' C '⟫'" (at level 80).
Inductive infer_type : env -> typed -> env -> IdSet.t -> cstrs -> Prop :=

| it_var : forall Γ x V T,
    lookup x Γ = Some T ->
    (*----------------------------------------------------*)
    Γ ⊨ (ﾍ x ⦂ V) ▷ (delete (ﾍ x ⦂ T) Γ) ⃒ ❲❳ ⟪[V ≖ T]⟫

| it_unit_l : forall Γ Γ' t V T χ C,
    Γ ⊨ (t ⦂ T) ▷ Γ' ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    Γ#(unit ⦂ V) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ ⟪C#(V ≖ I)⟫

| it_unit_r : forall Γ V,
    (*----------------------------------------------------*)
    Γ ⊨ (unit ⦂ V) ▷ Γ ⃒ ❲❳ ⟪[V ≖ I]⟫

| it_inl_l : forall Γ Γ' t1 t V T χ X1 X2 C,
    Γ#(t1 ⦂ ﾋ X1) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    Γ#(inl t1 ⦂ V) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ∪❲X1❳∪❲X2❳ ⟪C#(V ≖ ﾋ X1 ⊕ ﾋ X2)⟫

| it_inl_r : forall Γ Γ' t1 V χ X1 X2 C,
    Γ ⊨ (t1 ⦂ ﾋ X1) ▷ Γ' ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    Γ ⊨ (inl t1 ⦂ V) ▷ Γ' ⃒ χ∪❲X1❳∪❲X2❳ ⟪C#(V ≖ ﾋ X1 ⊕ ﾋ X2)⟫

| it_tensor_l : forall Γ Γ' t1 t2 t V T χ X1 X2 C,
    Γ#(t1 ⦂ ﾋ X1)#(t2 ⦂ ﾋ X2) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    Γ#(t1 × t2 ⦂ V) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ∪❲X1❳∪❲X2❳ ⟪C#(V ≖ ﾋ X1 ⊗ ﾋ X2)⟫

| it_tensor_r : forall Γ Γ' Γ'' t1 t2 V χ1 χ2 X1 X2 C1 C2,
    Γ ⊨ (t1 ⦂ ﾋ X1) ▷ Γ' ⃒ χ1 ⟪C1⟫ -> Γ' ⊨ (t2 ⦂ ﾋ X2) ▷ Γ'' ⃒ χ2 ⟪C2⟫ ->
    (*----------------------------------------------------*)
    Γ ⊨ (t1 × t2 ⦂ V) ▷ Γ'' ⃒ χ1∪χ2∪❲X1❳∪❲X2❳ ⟪C1++C2#(V ≖ ﾋ X1 ⊗ ﾋ X2)⟫

| it_arrow_l : forall Γ Γ' Γ'' t1 t2 t V T χ1 χ2 X1 X2 C1 C2,
    Γ ⊨ (t1 ⦂ ﾋ X1) ▷ Γ' ⃒ χ1 ⟪C1⟫ ->
    Γ'#(t2 ⦂ ﾋ X2) ⊨ (t ⦂ T) ▷ Γ'' ⃒ χ2 ⟪C2⟫ ->
    (*----------------------------------------------------*)
    Γ#(t1 ↦ t2 ⦂ V) ⊨ (t ⦂ T) ▷ Γ'' ⃒ χ1∪χ2∪❲X1❳∪❲X2❳ ⟪C1++C2#(V ≖ ﾋ X1 ⊸ ﾋ X2)⟫

| it_arrow_r : forall Γ Γ' t1 t2 V χ X1 X2 C,
    Γ#(t1 ⦂ ﾋ X1) ⊨ (t2 ⦂ ﾋ X2) ▷ Γ' ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    Γ ⊨ (t1 ↦ t2 ⦂ V) ▷ Γ' ⃒ χ ⟪C#(V ≖ ﾋ X1 ⊸ ﾋ X2)⟫

| it_fold_l : forall Γ Γ' u t V T U χ Y C,
    Γ#(u ⦂ (Y~>Y∾U)U) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ ⟪C⟫ ->
    Y ∉ χ ->
    (*----------------------------------------------------*)
    Γ#(fld[Y∾U] u ⦂ V) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ ⟪C#(V ≖ Y∾U)⟫

| it_fold_r : forall Γ Γ' t V T χ X C,
    Γ ⊨ (t ⦂ (X~>X∾T)T) ▷ Γ' ⃒ χ ⟪C⟫ ->
    X ∉ χ ->
    (*----------------------------------------------------*)
    Γ ⊨ (fld[X∾T] t ⦂ V) ▷ Γ' ⃒ χ ⟪C#(V ≖ X∾T)⟫

| it_trace_l : forall Γ Γ' t u V T U χ X1 X2 C,
    Γ#(u ⦂ (U ⊕ ﾋ X1) ⊸ (U ⊕ ﾋ X2)) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    Γ#(trace[U] u ⦂ V) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ∪❲X1❳∪❲X2❳ ⟪C#(V ≖ ﾋ X1 ⊸ ﾋ X2)⟫

| it_trace_r : forall Γ Γ' t V T χ X1 X2 C,
    Γ ⊨ (t ⦂ (T ⊕ ﾋ X1) ⊸ (T ⊕ ﾋ X2)) ▷ Γ' ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    Γ ⊨ (trace[T] t ⦂ V) ▷ Γ' ⃒ χ∪❲X1❳∪❲X2❳ ⟪C#(V ≖ ﾋ X1 ⊸ ﾋ X2)⟫

| it_lin_l : forall Γ Γ' t1 t2 t V T χ1 χ2 X C1 C2,
    Γ#(t1 ⦂ ﾋ X) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ1 ⟪C1⟫ ->
    Γ#(t2 ⦂ ﾋ X) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ2 ⟪C2⟫ ->
    is_empty (χ1∩χ2) = true ->
    (*----------------------------------------------------*)
    Γ#(t1 ∥ t2 ⦂ V) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ1∪χ2∪❲X❳ ⟪C1++C2#(V ≖ ﾋ X)⟫

| it_lin_r : forall Γ Γ' t1 t2 V χ1 χ2 X C1 C2,
    Γ ⊨ (t1 ⦂ ﾋ X) ▷ Γ' ⃒ χ1 ⟪C1⟫ ->
    Γ ⊨ (t2 ⦂ ﾋ X) ▷ Γ' ⃒ χ2 ⟪C1⟫ ->
    is_empty (χ1∩χ2) = true ->
    (*----------------------------------------------------*)
    Γ ⊨ (t1 ∥ t2 ⦂ V) ▷ Γ' ⃒ χ1∪χ2∪❲X❳ ⟪C1++C2#(V ≖ ﾋ X)⟫

| it_comp_l : forall Γ Γ' t1 t2 t V T χ X1 X2 X3 C,
    Γ#(t1 ⦂ ﾋ X1 ⊸ ﾋ X2)#(t2 ⦂ ﾋ X2 ⊸ ﾋ X3) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    Γ#(t1 ⨾ t2 ⦂ V) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ∪❲X1❳∪❲X2❳∪❲X3❳ ⟪C#(V ≖ ﾋ X1 ⊸ ﾋ X3)⟫

| it_comp_r : forall Γ Γ' Γ'' t1 t2 V χ1 χ2 X1 X2 X3 C1 C2,
    Γ ⊨ (t1 ⦂ ﾋ X1 ⊸ ﾋ X2) ▷ Γ' ⃒ χ1 ⟪C1⟫ ->
    Γ' ⊨ (t2 ⦂ ﾋ X2 ⊸ ﾋ X3) ▷ Γ'' ⃒ χ2 ⟪C2⟫ ->
    (*----------------------------------------------------*)
    Γ ⊨ (t1 ⨾ t2 ⦂ V) ▷ Γ'' ⃒ χ1∪χ2∪❲X1❳∪❲X2❳∪❲X3❳ ⟪C1++C2#(V ≖ ﾋ X1 ⊸ ﾋ X3)⟫

| it_dagger_l : forall Γ Γ' u t V T χ X1 X2 C,
    Γ#(u ⦂ ﾋ X2 ⊸ ﾋ X1) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    Γ#(† u ⦂ V) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ∪❲X1❳∪❲X2❳ ⟪C#(V ≖ ﾋ X1 ⊸ ﾋ X2)⟫

| it_dagger_r : forall Γ Γ' t V χ X1 X2 C,
    Γ ⊨ (t ⦂ ﾋ X2 ⊸ ﾋ X1) ▷ Γ' ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    Γ ⊨ (t ⦂ V) ▷ Γ' ⃒ χ∪❲X1❳∪❲X2❳ ⟪C#(V ≖ ﾋ X1 ⊸ ﾋ X2)⟫

| it_id_l : forall Γ Γ' t V T χ X C,
    Γ ⊨ (t ⦂ T) ▷ Γ' ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    Γ#(identity ⦂ V) ⊨ (t ⦂ T) ▷ Γ' ⃒ χ∪❲X❳ ⟪C#(V ≖ ﾋ X ⊸ ﾋ X)⟫

| it_id_r : forall Γ V X,
    (*----------------------------------------------------*)
    Γ ⊨ (identity ⦂ V) ▷ Γ ⃒ ❲X❳ ⟪[V ≖ ﾋ X ⊸ ﾋ X]⟫

where "Γ '⊨' c '▷' Θ '⃒' χ '⟪' C '⟫'" := (infer_type Γ c Θ χ C).

Reserved Notation "'⊨' c '⃒' χ '⟪' C '⟫'" (at level 80).
Inductive infer_expr_type : (ex * ty) -> IdSet.t -> cstrs -> Prop :=
| it_tm : forall t T χ C,
    [] ⊨ (t ⦂ T) ▷ [] ⃒ χ ⟪C⟫ ->
    (*----------------------------------------------------*)
    ⊨ (ｼ t ⦂ T) ⃒ χ ⟪C⟫

| it_app : forall e t1 V χ1 χ2 X1 X2 C1 C2,
    ⊨ (e ⦂ ﾋ X1 ⊸ ﾋ X2) ⃒ χ1 ⟪C1⟫ ->
    [] ⊨ (t1 ⦂ ﾋ X1) ▷ [] ⃒ χ2 ⟪C2⟫ ->
    (*----------------------------------------------------*)
    ⊨ (e @ t1 ⦂ V) ⃒ χ1∪χ2∪❲X1❳∪❲X2❳ ⟪C1++C2#(V ≖ ﾋ X2)⟫

where "'⊨' c '⃒' χ '⟪' C '⟫'" := (infer_expr_type c χ C).

Definition sbst_pair (X : id) (T : ty) (p:ty*ty) :=
  let (T1, T2) := p in (ty_sbst T X T1, ty_sbst T X T2).

Definition cons_opt (X : id) (T : ty) (l : option (IdMap.t ty)) :=
  match l with
  | None => None
  | Some s => Some (IdMap.add X T s)
  end.

Definition unify_sbst unif X T l :=
  if mem X (ty_vars T) then None else cons_opt X T (unif (map (sbst_pair X T) l)).

Fixpoint unify1 (unif : list (ty * ty) -> option (IdMap.t ty)) (h : nat) (l : list (ty * ty)) : option (IdMap.t ty) :=
  match h with
  | 0 => None
  | S h' =>
      match l with
      | [] => Some (@IdMap.empty ty)
      | (ﾋ X, ﾋ Y) :: l' =>
          if beq_id X Y then unify1 unif h' l' else unify_sbst unif X (ﾋ Y) l'
      | (ﾋ X, T) :: l' =>
          unify_sbst unif X T l'
      | (T, ﾋ X) :: l' =>
          unify_sbst unif X T l'
      | (I, I) :: l' => unify1 unif h' l'
      | (T1 ⊕ T2, U1 ⊕ U2) :: l' => unify1 unif h' ((T1, U1) :: (T2, U2) :: l')
      | (T1 ⊗ T2, U1 ⊗ U2) :: l' => unify1 unif h' ((T1, U1) :: (T2, U2) :: l')
      | (T1 ⊸ T2, U1 ⊸ U2) :: l' => unify1 unif h' ((T1, U1) :: (T2, U2) :: l')
      | (X ∾ T', Y ∾ U') :: l' => unify1 unif h' ((ﾋ X, ﾋ Y) :: (T', U') :: l')
      | _ => None
      end
  end.

Fixpoint size_pairs (l : list (ty * ty)) :=
  match l with
  | nil           => 0
  | (T1,T2) :: l' => (ty_size T1) + (ty_size T2) + (size_pairs l')
  end.

Fixpoint unify2 (h:nat) (l : list (ty * ty)) : option (IdMap.t ty) :=
  match h with
  | O    => None
  | S h' => unify1 (unify2 h') (size_pairs l + 1) l
end.

Definition unify l := unify2 (size_pairs l + 1) l.

(* Fixpoint unify (cos: list (ty * ty)) : option (ty -> ty) := *)
(*   match cos with *)
(*   | [] => Some (fun x => x) *)
(*   | (T, U) :: cos' => *)
(*       if beq_ty T U *)
(*       then unify cos' *)
(*       else *)
(*         match (T, U) with *)
(*         | (ﾋ X, _) => *)
(*             let s := X~>U in *)
(*             let tmp := unify (list_map (same_pair_map s) cos') in *)
(*             match tmp with *)
(*             | Some sbst => Some (compose sbst s) *)
(*             | None => None *)
(*             end *)
(*         | (_, ﾋ X) => *)
(*             let s := X~>T in *)
(*             let tmp := unify (list_map (same_pair_map s) cos') in *)
(*             match tmp with *)
(*             | Some sbst => Some (compose sbst s) *)
(*             | None => None *)
(*             end *)
(*         | (I, I) => unify cos' *)
(*         | (T1 ⊕ T2, U1 ⊕ U2) => [(T1, U1) ; (T2, U2)] ++ unify cos' *)
(*         | (T1 ⊗ T2, U1 ⊗ U2) => unify ((T1, U1) :: (T2, U2) :: cos') *)
(*         | (T1 ⊸ T2, U1 ⊸ U2) => unify ((T1, U1) :: (T2, U2) :: cos') *)
(*         | (X ∾ T', Y ∾ U') => unify ((ﾋ X, ﾋ Y) :: (T', U') :: cos') *)
(*         | _ => None *)
(*         end *)
(*   end. *)

(* Inductive taged : Type :=
| tt_var    : ty -> id -> taged
| tt_unit   : ty -> taged
| tt_left   : ty -> taged -> taged
| tt_right  : ty -> taged -> taged
| tt_tensor : ty -> taged -> taged -> taged
| tt_arrow  : ty -> taged -> taged -> taged
| tt_fold   : ty -> ty -> taged -> taged
| tt_trace  : ty -> ty -> taged -> taged
| tt_lin    : ty -> taged -> taged -> taged
| tt_comp   : ty -> taged -> taged -> taged
| tt_flip   : ty -> taged -> taged
| tt_nix    : ty -> taged
| tt_id     : ty -> taged
. *)

Example X := fun n => (Id n).
Example qubit := I ⊕ I.
Example nott :=
  (inl unit ↦ inr unit)
∥ (inr unit ↦ inl unit).
Example swap :=
  (inl unit × inl unit ↦ inl unit × inl unit)
∥ (inl unit × inr unit ↦ inr unit × inl unit)
∥ (inr unit × inl unit ↦ inl unit × inr unit)
∥ (inr unit × inr unit ↦ inr unit × inr unit).
Example natt := (X 0) ∾ (I ⊕ ﾋ(X 0)).
Example one := fld[natt] inr fld[natt] inl unit.

Compute [] ⊨ (one ⦂ ﾋ(Id 0)) ▷ [] ⃒ empty ⟪ [] ⟫.

(* 型のタグを持つデータ型を作成(taged_term) *)
(* unifyで得られた型の等式集合から項の全てにタグ付け *)

Inductive space : Type :=
| sp_empty : space
| sp_type  : ty -> space
| sp_sum   : space -> space -> space
| sp_konst :  -> space
.

Fixpoint projection (trm : tm)

Fixpoint patterns (trm : tm) : list sp :=
  match trm with
  | ﾍ x => [ﾍ x]
  | unit => []
  | inl t => []

(* subspace     (must) *)
(* projection   (must) *)
(* subtype      (option) *)
(* sig          (must) *)
(* decomposable (must) *)
(* decompose    (must) *)

(* ⊖ *)
Fixpoint subtract (a b : sp) :=
  match (a,b) with
  | (sp_nix,   sp_var x) => sp_nix
  | (sp_var x, sp_nix)   => sp_var x
  | 

End TyJudge.

Module TyEval.
Import ASTree.

Definition singleton {X:Type} := fun k e => IdMap.add k e (@IdMap.empty X).

(* Left priority in case of overlapping *)
Definition union_l {X:Type} (s1 s2 : IdMap.t X) : IdMap.t X :=
  IdMap.fold (fun k e => IdMap.add k e) s1 s2.

Definition union {X:Type} := @union_l X.

(* Right priority in case of overlapping *)
Definition union_r {X:Type} (s1 s2 : IdMap.t X) : IdMap.t X :=
  IdMap.fold (fun k e => IdMap.add k e) s2 s1.

Definition tenv := option (IdMap.t tm).

Definition union_and (s1 s2 : tenv) : tenv :=
  match (s1, s2) with
  | (Some e1, Some e2) => Some (union e1 e2)
  | (Some e1, None)    => None
  | (None   , Some e2) => None
  | (None   , None)    => None
  end.

Definition union_or (s1 s2 : tenv) : tenv :=
  match (s1, s2) with
  | (Some e1, Some e2) => Some (union e1 e2)
  | (Some e1, None)    => s1
  | (None   , Some e2) => s2
  | (None   , None)    => None
  end.

Fixpoint mtch_sub (h:nat) (tm1 tm2 : tm) : tenv :=
  match h with
  | O    => None
  | S h' =>
      match (tm1, tm2) with
      | (ﾍ x, t) => Some (singleton x t)
      | (unit, unit) => Some (@IdMap.empty tm)
      | (inl t, inl u) => mtch_sub h' t u
      | (inl t, inr u) => None
      | (inr t, inl u) => None
      | (inr t, inr u) => mtch_sub h' t u
      | (t1 × t2, u1 × u2) => union_and (mtch_sub h' t1 u1) (mtch_sub h' t2 u2)
      | (t1 ↦ t2, u1 ↦ u2) => union_and (mtch_sub h' t1 u1) (mtch_sub h' t2 u2)
      | (fld[T] t, fld[U] u) => mtch_sub h' t u
      | (trace[T] t, trace[U] u) => mtch_sub h' t u
      | (t, u1 ∥ u2) => union_or (mtch_sub h' t u1) (mtch_sub h' t u2)
      | (t1 ∥ t2, u) => union_or (mtch_sub h' t1 u) (mtch_sub h' t2 u)
      | (t1 ⨾ t2, u1 ⨾ u2) => union_and (mtch_sub h' t1 u1) (mtch_sub h' t2 u2)
      | († t, † u) => mtch_sub h' t u
      | (identity, identity) => Some (@IdMap.empty tm)
      | (nix, nix) => Some (@IdMap.empty tm)
      | _ => None
      end
  end.

Definition mtch (tm1 tm2 : tm) : tenv := mtch_sub (tm_size tm1 + tm_size tm2) tm1 tm2.

Fixpoint sbst (m : tenv) (trm : tm) : (tm * tenv) :=
  match m with
  | None   => (nix, None)
  | Some e =>
      match trm with
      | ﾍ x =>
          match IdMap.find x e with
          | None   => (nix, m)
          | Some t => (t, Some (IdMap.remove x e))
          end
      | unit => (unit, Some (@IdMap.empty tm))
      | inl t => match sbst m t with (t', m') => (inl t', m') end
      | inr t => match sbst m t with (t', m') => (inr t', m') end
      | t1 × t2 =>
          match sbst m t1 with
            (t1', m') =>
              match sbst m' t2 with
                (t2', m'') => (t1' × t2', m'')
              end
          end
      | t1 ↦ t2 =>
          match sbst m t1 with
            (t1', m') =>
              match sbst m' t2 with
                (t2', m'') => (t1' × t2', m'')
              end
          end
      | fld[T] t => match sbst m t with (t', m') => (fld[T] t', m') end
      | trace[T] t => match sbst m t with (t', m') => (trace[T] t', m') end
      | t1 ∥ t2 =>
          match sbst m t1 with
            (t1', m') =>
              match sbst m' t2 with
                (t2', m'') => (t1' ∥ t2', m'')
              end
          end
      | t1 ⨾ t2 =>
          match sbst m t1 with
            (t1', m') =>
              match sbst m' t2 with
                (t2', m'') => (t1' ⨾ t2', m'')
              end
          end
      | † t => match sbst m t with (t', m') => († t', m') end
      | identity => (identity, Some (@IdMap.empty tm))
      | nix => (nix, Some (@IdMap.empty tm))
      end
  end.

Reserved Notation "tm1 '≡' tm2" (at level 80).
Inductive tequiv : tm -> tm -> Prop :=
| teq_sym : forall t1 t2,
    t1 ∥ t2 ≡ t2 ∥ t1
| teq_unit :
    unit ≡ unit
| teq_var : forall x,
    ﾍ x ≡ ﾍ x
| teq_inl : forall t t',
    t ≡ t' ->
    inl t ≡ inl t'
| teq_inr : forall t t',
    t ≡ t' ->
    inr t ≡ inr t'
| teq_tensor : forall t1 t2 t1' t2',
    t1 ≡ t1' ->
    t2 ≡ t2' ->
    t1 × t2 ≡ t1' × t2'
| teq_arrow : forall t1 t2 t1' t2',
    t1 ≡ t1' ->
    t2 ≡ t2' ->
    t1 ↦ t2 ≡ t1' ↦ t2'
where "tm1 '≡' tm2" := (tequiv tm1 tm2).


Fixpoint transpose (t:tm) : tm :=
  match t with
  | † (t1 ~> t2)   => t2 ~> t1
  | † (t1 ∥ t2)   => (transpose t1) ∥ (transpose t2)
  | † (trace T t) => trace T (transpose t)
  | _ => t
  end
.

End TyEval.
End Omnirev.

Inductive tm_step : (list (id * tm) * tm) -> (list (id * tm) * tm) -> Prop :=
| tmst_var : forall Θ x t,
    (* tm_step ((x,t)::nil,(ﾍ x)) (nil,t) *)
    tm_step ((x, t) :: Θ, (ﾍ x)) (Θ, t)

| tmst_unit :
    (* forall Θ, tm_step (Θ,i) (Θ,i) *)
    tm_step (nil, i) (nil, i)

| tmst_left : forall Θ Ξ t1 u1,
    tm_step (Θ, t1) (Ξ, u1) ->
    tm_step (Θ, inl t1) (Ξ, inl u1)

| tmst_right : forall Θ Ξ t2 u2,
    tm_step (Θ, t2) (Ξ, u2) ->
    tm_step (Θ, inr t2) (Ξ, inr u2)

| tmst_tensor : forall Θ1 Θ2 Ξ1 Ξ2 t1 t2 u1 u2,
    tm_step (Θ1, t1) (Ξ1, u1) ->
    tm_step (Θ2, t2) (Θ2, u2) ->
    tm_step ((Θ1 ++ Θ2), (t1 × t2)) ((Ξ1 ++ Ξ2), (u1 × u2))

| 要修正tmst_arrow : forall Θ1 Θ2 Ξ1 Ξ2 t1 t2 u1 u2,
    tm_step (Θ1, t1) (Ξ1, u1) ->
    tm_step (Θ2, t2) (Ξ2, u2) ->
    tm_step ((Θ1 ++ Θ2), (t1 ~> t2)) ((Ξ1 ++ Ξ2), (u1 ~> u2))

| tmst_fold : forall Θ Ξ t u,
    tm_step (Θ, t) (Ξ, u) ->
    tm_step (Θ, fld t) (Ξ, fld u)

| tmst_lin : forall Θ Ξ t1 t2 u1 u2,
    tm_step (Θ, t1) (Ξ, u1) ->
    tm_step (Θ, t2) (Ξ, u2) ->
    tm_step (Θ, t1 ∥ t2) (Ξ, u1 ∥ u2)
.
