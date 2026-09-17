module Unification where

import Common
import Control.Exception
import Data.IntMap.Strict qualified as IM
import Evaluation
import Metacontext
import Syntax
import Value

--------------------------------------------------------------------------------

data UnifyError = UnifyError
  deriving stock (Show)
  deriving anyclass (Exception)

--------------------------------------------------------------------------------

-- | partial renaming from Γ to Δ
data PartialRenaming = PRen
  { -- | size of Γ
    dom :: Lvl,
    -- | size of Δ
    cod :: Lvl,
    -- | mapping from Δ vars to Γ vars
    ren :: IM.IntMap Lvl
  }

-- | Lifting a partial renaming over an extra bound variable.
--   Given (σ : PRen Γ Δ), (lift σ : PRen (Γ, x : A[σ]) (Δ, x : A))
lift :: PartialRenaming -> PartialRenaming
lift (PRen dom cod ren) =
  PRen (dom + 1) (cod + 1) (IM.insert (coerce cod) dom ren)

-- | @invert : (Γ : Cxt) → (spine : Sub Γ Δ) → PRen Δ Γ@
invert :: Lvl -> Sp -> IO PartialRenaming
invert gamma sp = do
  let go :: Sp -> IO (Lvl, IM.IntMap Lvl)
      go [] = pure (0, mempty)
      go (sp :> t) = do
        (dom, ren) <- go sp
        case force t of
          VVar (Lvl x) | IM.notMember x ren -> pure (dom + 1, IM.insert x dom ren)
          -- choice can't be inverted
          _ -> throwIO UnifyError

  (dom, ren) <- go sp
  pure $ PRen dom gamma ren

-- | Perform the partial renaming on rhs, while also checking for "m" occurrences.
rename :: MetaVar -> PartialRenaming -> Val -> IO Tm
rename m pren v = go pren v
  where
    goSp :: PartialRenaming -> Tm -> Sp -> IO Tm
    goSp _ t [] = pure t
    goSp pren t (sp :> u) = App <$> goSp pren t sp <*> go pren u

    go :: PartialRenaming -> Val -> IO Tm
    go pren t = case force t of
      VFlex m' sp
        | m == m' -> throwIO UnifyError -- occurs check
        | otherwise -> goSp pren (Meta m') sp
      VRigid (Lvl x) sp -> case IM.lookup x pren.ren of
        Nothing -> throwIO UnifyError -- scope error ("escaping variable" error)
        Just x' -> goSp pren (Var $ lvl2Ix pren.dom x') sp
      VLam x t -> Lam x <$> go (lift pren) (t $ VVar pren.cod)
      VPi x a b -> Pi x <$> go pren a <*> go (lift pren) (b $ VVar pren.cod)
      VU -> pure U
      VChoice c t u -> Choice c <$> go pren t <*> go pren u

{-
Wrap a term in lambdas.
-}
lams :: Lvl -> Tm -> Tm
lams l = go 0
  where
    go x t | x == l = t
    go x t = Lam (NX (l + 1)) $ go (x + 1) t

--       Γ      ?α         sp       rhs
solve :: Lvl -> MetaVar -> Sp -> Val -> IO ()
solve gamma m sp rhs = do
  pren <- invert gamma sp
  rhs <- rename m pren rhs
  let solution = eval [] $ lams pren.dom rhs
  writeMeta m solution

class Monad m => UnifyMonad m where
  trySolve  :: Lvl -> MetaVar -> Sp -> Val -> m ()
  tryRefine :: ChoiceVar -> ChoiceEntry -> m ()
  stuck    :: m ()
  mismatch :: m ()

instance UnifyMonad IO where
  trySolve = solve
  tryRefine = writeChoice
  stuck    = throwIO UnifyError
  mismatch = throwIO UnifyError -- rigid mismatch error

data PureUnify a = Conv a | Stuck | Anti
  deriving stock Functor

instance Applicative PureUnify where
  pure = Conv
  Conv f <*> x = f <$> x
  -- TODO: We might be able to have
  -- > _ <*> Anti = Anti
  -- it's not clear to me exactly when this safe
  Stuck  <*> _ = Stuck
  Anti   <*> _ = Anti

instance Monad PureUnify where
  Conv x >>= f = f x
  Stuck  >>= _ = Stuck
  Anti   >>= _ = Anti

instance UnifyMonad PureUnify where
  trySolve _ _ _ _ = Stuck
  tryRefine _ _    = Stuck
  stuck            = Stuck
  mismatch         = Anti

isAnti :: PureUnify a -> Bool
isAnti Anti = True
isAnti _    = False

antiUnifies :: Lvl -> Val -> Val -> Bool
antiUnifies l v1 v2 = isAnti $ unify l v1 v2

unifySp :: UnifyMonad m => Lvl -> Sp -> Sp -> m ()
unifySp l sp sp' = case (sp, sp') of
  ([], []) -> pure ()
  (sp :> t, sp' :> t') -> unifySp l sp sp' >> unify l t t'
  _ -> mismatch -- rigid mismatch error

unify :: UnifyMonad m => Lvl -> Val -> Val -> m ()
unify l t u = case (force t, force u) of
  (VLam _ t, VLam _ t') -> unify (l + 1) (t $ VVar l) (t' $ VVar l)
  (t, VLam _ t') -> unify (l + 1) (t $$ VVar l) (t' $ VVar l)
  (VLam _ t, t') -> unify (l + 1) (t $ VVar l) (t' $$ VVar l)
  (VU, VU) -> pure ()
  (VPi _ a b, VPi _ a' b') -> unify l a a' >> unify (l + 1) (b $ VVar l) (b' $ VVar l)
  (VRigid x sp, VRigid x' sp') | x == x' -> unifySp l sp sp'
  (VFlex m sp, VFlex m' sp') | m == m' -> unifySp l sp sp'
  (VFlex m sp, t') -> trySolve l m sp t'
  (t, VFlex m' sp') -> trySolve l m' sp' t
  -- TODO: Could probably do something smarter here...
  -- (VChoice c tl tr, VChoice c' tl' tr') = error "TODO"
  (VChoice _ tl tr, t')
    | antiUnifies l tl t' && antiUnifies l tr t'
    -> mismatch
  (VChoice c tl tr, t')
    | antiUnifies l tl t'
    -> tryRefine c R >> unify l tr t'
  (VChoice c tl tr, t')
    | antiUnifies l tr t'
    -> tryRefine c L >> unify l tl t'
  (VChoice {}, _)
    -> stuck -- We cannot make progress (ideally would postpone)
  (t, VChoice c tl' tr') -> unify l (VChoice c tl' tr') t
  _ -> mismatch
