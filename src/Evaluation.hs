module Evaluation where

import Common
import Metacontext
import Syntax
import Value

--------------------------------------------------------------------------------

eval :: Env -> Tm -> Val
eval env = \case
  Var (Ix x) -> env !! x
  Meta m -> vMeta m
  Choice c tl tr -> vChoice c (eval env tl) (eval env tr)
  U -> VU
  Pi x a b -> VPi x (eval env a) \ ~v -> eval (env :> v) b
  Lam x t -> VLam x \v -> eval (env :> v) t
  App t u -> eval env t $$ eval env u
  InsertedMeta m l -> vInsertedMeta m l

vMeta :: MetaVar -> Val
vMeta m = case lookupMeta m of
  Unsolved -> VMeta m
  Solved v -> v

vChoice :: ChoiceVar -> Val -> Val -> Val
vChoice c ~tl ~tr = case lookupChoice c of
  L -> tl
  R -> tr
  B -> VChoice c tl tr

($$) :: Val -> Val -> Val
t $$ ~u = case t of
  VLam _ f -> f u
  VRigid x sp -> VRigid x (sp :> u)
  VFlex m sp -> VFlex m (sp :> u)
  VChoice c tl tr -> VChoice c (tl $$ u) (tr $$ u)
  VU; VPi {} -> error "impossible"

vAppSp :: Val -> Sp -> Val
vAppSp t [] = t
vAppSp t (sp :> u) = vAppSp t sp $$ u

vInsertedMeta :: MetaVar -> Lvl -> Val
vInsertedMeta m l = case lookupMeta m of
  Unsolved -> VFlex m (idSp l)
  Solved t -> do
    let go 0 = t
        go l = go (l - 1) $$ VVar (l - 1)
    go l

idSp :: Lvl -> Sp
idSp 0 = []
idSp l = idSp (l - 1) :> VVar (l - 1)

--------------------------------------------------------------------------------

force :: Val -> Val
force = \case
  VFlex m sp | Solved t <- lookupMeta m -> force (vAppSp t sp)
  VChoice c tl _ | L <- lookupChoice c -> force tl
  VChoice c _ tr | R <- lookupChoice c -> force tr
  t -> t

--------------------------------------------------------------------------------

quote :: Lvl -> Val -> Tm
quote l t = case force t of
  VRigid x sp -> quoteSp l (Var (lvl2Ix l x)) sp
  VFlex m sp -> quoteSp l (Meta m) sp
  VChoice c t u -> Choice c (quote l t) (quote l u)
  VU -> U
  VPi x a b -> Pi x (quote l a) (quote (l + 1) (b (VVar l)))
  VLam x t -> Lam x (quote (l + 1) (t (VVar l)))

quoteSp :: Lvl -> Tm -> Sp -> Tm
quoteSp _ h [] = h
quoteSp l h (sp :> u) = App (quoteSp l h sp) (quote l u)

nf :: Env -> Tm -> Tm
nf env t = quote (Lvl $ length env) (eval env t)
