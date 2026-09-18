module Syntax where

import Common

--------------------------------------------------------------------------------

data Tm
  = Var Ix
  | Meta MetaVar
  | Choice ChoiceVar Tm Tm
  | U
  | Pi Name Ty Ty
  | Lam Name Tm
  | App Tm Tm
  | InsertedMeta MetaVar Lvl
  deriving stock (Show)

type Ty = Tm

-- α-equivalence
deriving stock instance Eq Tm
