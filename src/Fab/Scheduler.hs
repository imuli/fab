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
  , verbose
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Class (MonadState, gets, modify)
import           Control.Monad.State.Strict (runStateT)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Proxy (Proxy(Proxy))
import           Fab.Core
import           Fab.Result (Result(Pure))
import           Fab.Store (HasFabStore, getConfig, getValidation, getValue, modifyValidation,
                     putValue)

-- | Constraints required for a scheduler.
type SchedulerC s t f = (Monad f, HasFabStore s f, MonadState (s f) (t f), MonadTrans t)

-- | A scheduler, takes a fabrication monad, and produces a 'Result' in some
-- 'MonadState' transformer.
type Scheduler s t f = forall a. SchedulerC s t f => FabT f a -> t f (Result a)

data PhaseInfo f
   = forall t. Fab f t => Fabricated t (Result (FabVal f t))
   | forall t. Fab f t => Validated t (Result (FabVal f t))
   | forall t. Fab f t => Invalidated t (Result (FabVal f t))
   | forall t. Fab f t => Fetched t (Result (FabVal f t))

-- | A recorder runs and can update the store after each phase.
type Recorder' s f = PhaseInfo f -> f (s f -> s f)

-- | A recorder runs, and updates the store, every time a key is built.
type Recorder s f = forall t. Fab f t => t -> Result (FabVal f t) -> f (s f -> s f)

norec :: Applicative f => Recorder s f
norec _ _ = pure id

-- | Single threaded complete and (essentially) minimal scheduler, which
-- respects 'Validation' information.
simple :: forall s t f. Monad f => Scheduler s t f
simple = simpleWith (const $ pure id) norec

verbose :: forall s t f. MonadIO f => Scheduler s t f
verbose = simpleWith ((id <$) . liftIO . dolog) norec
  where
    dolog (Fabricated k v)  = putStrLn $ "Fabricated "  <> show k <> " to " <> show v
    dolog (Validated k v)   = putStrLn $ "Validated "   <> show k <> " as " <> show v
    dolog (Invalidated k v) = putStrLn $ "Invalidated " <> show k <> " as " <> show v
    dolog (Fetched k v)     = putStrLn $ "Fetched " <> show k <> " as " <> show v

-- | The simple scheduler, paramaterized with a store-updating action to perform when keys are built.
simpleWith :: SchedulerC s t f => Recorder' s f -> Recorder s f -> FabT f a -> t f (Result a)
simpleWith gr r x = lift (runFabT x) >>= simpleResult gr r

simpleResult :: SchedulerC s t f => Recorder' s f -> Recorder s f -> FabResult f a -> t f (Result a)
simpleResult _  _ (Res res) = pure res
simpleResult gr r (Req req) = simpleResolve gr r req

-- FIXME It seems like the rest of these could be... simplier, or at least less verbose.
simpleResolve :: SchedulerC s t f => Recorder' s f -> Recorder s f -> Request f a -> t f (Result a)
simpleResolve gr r (Request req g) = case req of
       ForConfig -> gets (pure . getConfig) >>= simpleWith gr r . g
       ForCachedKey k -> do
         v <- simpleFetch gr k
         case sequence v of
              Nothing -> pure ()
              Just v' -> modify =<< lift (r k v')
         simpleWith gr r $ g v
       ForKey k -> do
         v <- simpleBuild gr k
         modify =<< (.) <$> lift (r k v) <*> lift (gr $ Fabricated k v)
         simpleWith gr r $ g v
simpleResolve gr r (ReqApp a b g) = do
         a' <- simpleResolve gr r a
         b' <- simpleResolve gr r b
         simpleWith gr r . g $ a' <*> b'

simpleFetch :: (SchedulerC s t f, Fab f k) => Recorder' s f -> k -> t f (Result (Maybe (FabVal f k)))
simpleFetch gr k = maybe (pure (pure Nothing)) check =<< gets (getValue k)
  where
    check v = do
      gets (getValidation k) >>= simpleWith gr norec . verify k v >>= \case
        Pure True -> do
          modify =<< lift (gr $ Fetched k v)
          pure (Just <$> v)
        _         -> pure (pure Nothing)

-- | Build a key with the simple scheduler.
simpleBuild :: (SchedulerC s t f, Fab f k) => Recorder' s f -> k -> t f (Result (FabVal f k))
simpleBuild gr k = maybe run check =<< gets (getValue k)
  where
    run = do
      v <- simpleWith gr (((modifyValidation k <$>) .) . record k) $ fabricate k
      fin <- lift $ finalize k v
      modify (putValue k v . modifyValidation k fin)
      pure v
    check v = do
      gets (getValidation k) >>= simpleWith gr norec . verify k v >>= \case
        Pure True -> do
          modify =<< lift (gr $ Validated k v)
          pure v
        _         -> do
          modify =<< lift (gr $ Invalidated k v)
          run

-- | Always rebuilds everything every time.
busy :: Monad f => FabT f a -> f (Result a)
busy x = fst <$> runStateT (simple x) Proxy
