module Metacontext
  ( MetaEntry (..),
    newMeta,
    readMeta,
    lookupMeta,
    writeMeta,
    ChoiceEntry (..),
    LR (..),
    newChoice,
    readChoice,
    lookupChoice,
    writeChoice,
    reset,
  )
where

import Common
import Data.IORef
import Data.IntMap.Strict qualified as IM
import System.IO.Unsafe
import Value

--------------------------------------------------------------------------------

data MetaEntry
  = Unsolved
  | Solved Val

nextMetaVar :: IORef MetaVar
nextMetaVar = unsafeDupablePerformIO (newIORef 0)
{-# NOINLINE nextMetaVar #-}

metaCtx :: IORef (IM.IntMap MetaEntry)
metaCtx = unsafeDupablePerformIO (newIORef mempty)
{-# NOINLINE metaCtx #-}

newMeta :: IO MetaVar
newMeta = do
  m <- readIORef nextMetaVar
  writeIORef nextMetaVar $! m + 1
  modifyIORef' metaCtx $ IM.insert (coerce m) Unsolved
  pure m

readMeta :: MetaVar -> IO MetaEntry
readMeta m = do
  ms <- readIORef metaCtx
  case IM.lookup (coerce m) ms of
    Just e -> pure e
    Nothing -> error "impossible"

lookupMeta :: MetaVar -> MetaEntry
lookupMeta = unsafeDupablePerformIO . readMeta

writeMeta :: MetaVar -> Val -> IO ()
writeMeta m sol = modifyIORef' metaCtx $ IM.insert (coerce m) (Solved sol)

--------------------------------------------------------------------------------

data LR = L | R
  deriving stock (Show)

data ChoiceEntry
  = CUnsolved [(ChoiceVar, LR)] [(ChoiceVar, LR)]
  | CSolved LR
  deriving stock (Show)

nextChoiceVar :: IORef ChoiceVar
nextChoiceVar = unsafeDupablePerformIO (newIORef 0)
{-# NOINLINE nextChoiceVar #-}

choiceCtx :: IORef (IM.IntMap ChoiceEntry)
choiceCtx = unsafeDupablePerformIO (newIORef mempty)
{-# NOINLINE choiceCtx #-}

newChoice :: [(ChoiceVar, LR)] -> [(ChoiceVar, LR)] -> IO ChoiceVar
newChoice constrL constrR = do
  m <- readIORef nextChoiceVar
  writeIORef nextChoiceVar $! m + 1
  modifyIORef choiceCtx $ IM.insert (coerce m) (CUnsolved constrL constrR)
  pure m

readChoice :: ChoiceVar -> IO ChoiceEntry
readChoice c = do
  cs <- readIORef choiceCtx
  case IM.lookup (coerce c) cs of
    Just e -> pure e
    Nothing -> error "impossible"

lookupChoice :: ChoiceVar -> ChoiceEntry
lookupChoice = unsafeDupablePerformIO . readChoice

writeChoice :: ChoiceVar -> LR -> IO ()
writeChoice c lr = do
  cs <- readIORef choiceCtx
  case IM.lookup (coerce c) cs of
    Just (CUnsolved constrL constrR) -> do
      modifyIORef' choiceCtx $ IM.insert (coerce c) (CSolved lr)
      case lr of
        L -> traverse_ (uncurry writeChoice) constrL
        R -> traverse_ (uncurry writeChoice) constrR
    Nothing; Just (CSolved {}) -> error "impossible"

--------------------------------------------------------------------------------

reset :: IO ()
reset = do
  writeIORef nextMetaVar 0
  writeIORef metaCtx mempty
  writeIORef nextChoiceVar 0
  writeIORef choiceCtx mempty
