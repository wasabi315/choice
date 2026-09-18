module Metacontext
  ( MetaEntry (..),
    newMeta,
    readMeta,
    lookupMeta,
    writeMeta,
    ChoiceEntry (..),
    newChoice,
    readChoice,
    lookupChoice,
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
  modifyIORef metaCtx $ IM.insert (coerce m) Unsolved
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

data ChoiceEntry = B | L | R

nextChoiceVar :: IORef ChoiceVar
nextChoiceVar = unsafeDupablePerformIO (newIORef 0)
{-# NOINLINE nextChoiceVar #-}

choiceCtx :: IORef (IM.IntMap ChoiceEntry)
choiceCtx = unsafeDupablePerformIO (newIORef mempty)
{-# NOINLINE choiceCtx #-}

newChoice :: IO ChoiceVar
newChoice = do
  m <- readIORef nextChoiceVar
  writeIORef nextChoiceVar $! m + 1
  modifyIORef choiceCtx $ IM.insert (coerce m) B
  pure m

readChoice :: ChoiceVar -> IO ChoiceEntry
readChoice c = do
  cs <- readIORef choiceCtx
  case IM.lookup (coerce c) cs of
    Just e -> pure e
    Nothing -> error "impossible"

lookupChoice :: ChoiceVar -> ChoiceEntry
lookupChoice = unsafeDupablePerformIO . readChoice

--------------------------------------------------------------------------------

reset :: IO ()
reset = do
  writeIORef nextMetaVar 0
  writeIORef metaCtx mempty
  writeIORef nextChoiceVar 0
  writeIORef choiceCtx mempty
