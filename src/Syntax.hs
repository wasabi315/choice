module Syntax where

import Common

--------------------------------------------------------------------------------

data Tm
  = Var Ix
  | Meta MetaVar
  -- Meta-level choices
  | Choice ChoiceVar ~Tm ~Tm
  -- Meta-level coercions
  | Coe Ty Ty Tm
  | U
  | Pi Name Ty Ty
  | Lam Name Tm
  | App Tm Tm
  | InsertedMeta MetaVar Lvl
  deriving stock (Show)

type Ty = Tm

-- α-equivalence
deriving stock instance Eq Tm
