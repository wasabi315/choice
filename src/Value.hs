module Value where

import Common

--------------------------------------------------------------------------------

data Val
  = VRigid Lvl Sp
  | VFlex MetaVar FlexHead Sp
  | VChoice ChoiceVar ~Val ~Val -- Meta-level case
  | VU
  | VPi Name VTy (Val -> VTy)
  | VLam Name (Val -> Val)
  | VErr
  -- ^ Error values (e.g. meta-level coercion over inconsistent equations)
  --   To handle heteregeneous choices, we may need to catch such inconsistent
  --   coercions

data Elim
  = SApp Val
  | SCoe VTy VTy -- Meta-level coercion

data FlexHead
  = FHMeta
    -- ^ Meta-headed
  | FHCoe VTy Sp Val
    -- ^ Coe-headed (target type is meta-headed)

type VTy = Val

type Sp = [Elim]

type Env = [Val]

pattern VVar x = VRigid x []

pattern VMeta m = VFlex m FHMeta []

pattern VCoe a bm bsp t = VFlex bm (FHCoe a bsp t) []
