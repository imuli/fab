{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Schedulers for fabrication.
-}

module Fab.Scheduler
  ( Scheduler
  , SchedulerC
  , Recorder
  , busy
  , simple
  , simpleWith
  , simpleBuild
  ) where

import           Control.Monad.State.Class (MonadState, gets, modify)
import           Control.Monad.State.Strict (runStateT)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Proxy (Proxy(Proxy))
import           Fab.Core
import           Fab.Result (Result(Pure))
import           Fab.Store (HasFabStore, getConfig, getRefab, getValue, modifyRefab, putValue)

-- | Constraints required for a scheduler.
type SchedulerC s t f = (Monad f, HasFabStore s f, MonadState (s f) (t f), MonadTrans t)

-- | A scheduler, takes a fabrication monad, and produces a 'Result' in some
-- 'MonadState' transformer.
type Scheduler s t f = forall a. SchedulerC s t f => FabT f a -> t f (Result a)

-- | A recorder runs, and updates the store, every time a key is built.
type Recorder s f = forall t. Fab f t => t -> Result (FabVal f t) -> f (s f -> s f)

norec :: Applicative f => Recorder s f
norec _ _ = pure id

-- | Single threaded complete and (essentially) minimal scheduler, which
-- respects 'Refab' information.
simple :: forall s t f. Monad f => Scheduler s t f
simple = simpleWith norec

-- | The simple scheduler, paramaterized with a store-updating action to perform when keys are built.
simpleWith :: SchedulerC s t f => Recorder s f -> FabT f a -> t f (Result a)
simpleWith r x = lift (runFabT x) >>= simpleResult r

simpleResult :: SchedulerC s t f => Recorder s f -> FabResult f a -> t f (Result a)
simpleResult _ (Res res) = pure res
simpleResult r (Req req) = simpleResolve r req

-- FIXME It seems like the rest of these could be... simplier, or at least less verbose.
simpleResolve :: SchedulerC s t f => Recorder s f -> Request f a -> t f (Result a)
simpleResolve r (Request req g) = case req of
       ForConfig -> gets (pure . getConfig) >>= simpleWith r . g
       ForCachedKey k -> do
         v <- simpleFetch k
         case sequence v of
              Nothing -> pure ()
              Just v' -> modify =<< lift (r k v')
         simpleWith r $ g v
       ForKey k -> do
         v <- simpleBuild k
         modify =<< lift (r k v)
         simpleWith r $ g v
simpleResolve r (ReqApp a b g) = do
         a' <- simpleResolve r a
         b' <- simpleResolve r b
         simpleWith r . g $ a' <*> b'

simpleFetch :: (SchedulerC s t f, Fab f k) => k -> t f (Result (Maybe (FabVal f k)))
simpleFetch k = maybe (pure (pure Nothing)) check =<< gets (getValue k)
  where
    check v = do
      gets (getRefab k) >>= simpleWith norec . verify k v >>= \case
        Pure True -> pure (Just <$> v)
        _         -> pure (pure Nothing)

-- | Build a key with the simple scheduler.
simpleBuild :: (SchedulerC s t f, Fab f k) => k -> t f (Result (FabVal f k))
simpleBuild k = maybe run check =<< gets (getValue k)
  where
    run = do
      (v) <- simpleWith norec $ fabricate k
      fin <- lift $ finalize k v
      modify (putValue k v . modifyRefab k fin)
      pure v
    check v = do
      gets (getRefab k) >>= simpleWith norec . verify k v >>= \case
        Pure True -> pure v
        _         -> run

-- | Always rebuilds everything every time.
busy :: Monad f => FabT f a -> f (Result a)
busy x = fst <$> runStateT (simple x) Proxy
