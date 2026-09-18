module Common
  ( module Common,
    module Control.Applicative,
    module Control.Monad,
    module Data.Foldable,
    module Data.Traversable,
    module Data.Coerce,
  )
where

import Control.Applicative
import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Traversable

--------------------------------------------------------------------------------

newtype Ix = Ix Int
  deriving newtype (Eq, Ord, Show, Num, Enum, Bounded)

newtype Lvl = Lvl Int
  deriving newtype (Eq, Ord, Show, Num, Enum, Bounded)

newtype MetaVar = MetaVar Int
  deriving newtype (Eq, Ord, Show, Num, Enum, Bounded)

newtype ChoiceVar = ChoiceVar Int
  deriving newtype (Eq, Ord, Show, Num, Enum, Bounded)

data Name
  = Name String
  | NChoice ChoiceVar Name Name
  | NX Lvl
  deriving stock (Show)

instance Eq Name where
  ~_ == ~_ = True

instance Ord Name where
  compare ~_ ~_ = EQ

lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix (Lvl l) (Lvl x) = Ix (l - x - 1)

pattern xs :> x <- x : xs
  where
    xs :> ~x = x : xs

{-# COMPLETE [], (:>) #-}
