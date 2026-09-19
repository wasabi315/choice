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
  Coe a b t -> vCoe (eval env a) (eval env b) (eval env t)
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
  B -> pushChoice c tl tr

-- | Types can be meta-headed, but never flexibly coerced
assertFHMeta :: FlexHead -> a -> a
assertFHMeta FHMeta     c = c
assertFHMeta (FHCoe {}) _ = error "impossible"

vCoe :: VTy -> VTy -> Val -> Val
vCoe a b t = case t of
  VLam x t' -> case (a, b) of
    (VPi _ a1 a2, VPi _ b1 b2) -> VLam x \u ->
      vCoe (a2 $ vCoe b1 a1 u) (b2 u) (t' $ vCoe b1 a1 u)
    (VPi {}, VFlex bm bh bsp) -> assertFHMeta bh $ VCoe a bm bsp t
    _                         -> VErr
  VChoice c l r -> VChoice c (vCoe a b l) (vCoe a b r)
  -- TODO: Coercions should compute on reflexivity
  -- This requires threading a |Lvl| through |eval| and calling into a
  -- a pure version of |Unify|
  -- For now, coercions are just handled lazily in unification, but
  -- https://andraskovacs.github.io/pdfs/wits26prez.pdf
  -- says this might be bad for term size...
  VRigid h sp   -> VRigid h (sp :> SCoe a b)
  VFlex  m h sp -> VFlex m h (sp :> SCoe a b)
  VPi {} -> case b of
    VU              -> t
    VFlex bm bh bsp -> assertFHMeta bh $ VCoe a bm bsp t
    _               -> VErr
  VU -> case b of
    VU              -> t
    VFlex bm bh bsp -> assertFHMeta bh $ VCoe a bm bsp t
    _               -> VErr

  VErr -> VErr

vAppSp :: Val -> Sp -> Val
vAppSp t [] = t
vAppSp t (sp :> SApp u)   = vAppSp t sp $$ u
vAppSp t (sp :> SCoe a b) = vCoe a b $ vAppSp t sp


($$) :: Val -> Val -> Val
t $$ ~u = case t of
  VLam _ f        -> f u
  VRigid x sp     -> VRigid x (sp :> SApp u)
  VFlex m h sp    -> VFlex m h (sp :> SApp u)
  VChoice c tl tr -> VChoice c (tl $$ u) (tr $$ u)
  VU; VPi {}      -> error "impossible"
  VErr            -> VErr

vInsertedMeta :: MetaVar -> Lvl -> Val
vInsertedMeta m l = case lookupMeta m of
  Unsolved -> VFlex m FHMeta $ idSp l
  Solved t -> do
    let go 0 = t
        go l = go (l - 1) $$ VVar (l - 1)
    go l

idSp :: Lvl -> Sp
idSp 0 = []
idSp l = idSp (l - 1) :> SApp (VVar $ l - 1)

--------------------------------------------------------------------------------

forceFH :: FlexHead -> Val -> Val
forceFH FHMeta          t = t
forceFH (FHCoe a bsp t) b = vCoe a (vAppSp b bsp) t

force :: Val -> Val
force = \case
  VFlex m h sp | Solved t <- lookupMeta m
    -> vAppSp (forceFH h t) sp
  VChoice c tl tr -> case lookupChoice c of
    L -> tl
    R -> tr
    B -> pushChoice c tl tr
  t -> t

-- assumes c is an unsolved choicevar
pushChoice :: ChoiceVar -> Val -> Val -> Val
pushChoice c t t' = case (force t, force t') of
  (VLam x t, VLam x' t') -> VLam (NChoice c x x') \v -> VChoice c (t v) (t' v)
  (VLam x t, t') -> VLam x \v -> VChoice c (t v) (t' $$ v)
  (t, VLam x' t') -> VLam x' \v -> VChoice c (t $$ v) (t' v)
  (VU, VU) -> VU
  (VPi x a b, VPi x' a' b') -> VPi (NChoice c x x') (VChoice c a a') \ ~v -> VChoice c (b v) (b' v)
  -- TODO: How should we handle nested choice here?
  (t, t') -> VChoice c t t'

--------------------------------------------------------------------------------

quoteFH :: Lvl -> FlexHead -> Tm -> Tm
quoteFH _ FHMeta t          = t
quoteFH l (FHCoe a bsp t) b = Coe (quote l a) (quoteSp l b bsp) (quote l t)

quote :: Lvl -> Val -> Tm
quote l t = case force t of
  VRigid x sp -> quoteSp l (Var (lvl2Ix l x)) sp
  VFlex m h sp -> quoteSp l (quoteFH l h (Meta m)) sp
  VChoice c t u -> Choice c (quote l t) (quote l u)
  VU -> U
  VPi x a b -> Pi x (quote l a) (quote (l + 1) (b (VVar l)))
  VLam x t -> Lam x (quote (l + 1) (t (VVar l)))
  VErr -> error "impossible"

quoteSp :: Lvl -> Tm -> Sp -> Tm
quoteSp _ h []               = h
quoteSp l h (sp :> SApp u)   = App (quoteSp l h sp) (quote l u)
quoteSp l h (sp :> SCoe a b) = Coe (quote l a) (quote l b) (quoteSp l h sp)

nf :: Env -> Tm -> Tm
nf env t = quote (Lvl $ length env) (eval env t)
