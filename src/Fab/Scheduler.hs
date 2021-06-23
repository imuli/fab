{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Schedulers for fabrication.
-}

module Fab.Scheduler
  ( Scheduler
  , Recorder
  , busy
  , simple
  , simpleWith
  ) where

import           Data.Functor ((<&>))
import           Data.Proxy (Proxy(Proxy))
import           Fab.Core
import           Fab.Result (Result(Pure, Throw))
import           Fab.Store (HasFabStore, getConfig, getRefab, getValue, modifyRefab, putValue)

-- | A scheduler, takes a fabrication store, a fabrication transfomer, and
-- provides a new store with the result.
type Scheduler s f = forall a. HasFabStore s f => s f -> FabT f a -> f (s f, Result a)

-- | A recorder runs, and updates the store, every time a key is built.
type Recorder s f = forall t. Fab f t => t -> Result (FabVal f t) -> f (s f -> s f)

norec :: Applicative f => Recorder s f
norec _ _ = pure id

-- | Single threaded complete and (essentially) minimal scheduler, which
-- respects 'Refab' information.
simple :: forall s f. Monad f => Scheduler s f
simple = simpleWith norec

-- | The simple scheduler, paramaterized with a store-updating action to perform when keys are built.
simpleWith :: (Monad f, HasFabStore s f) => Recorder s f -> s f -> FabT f a -> f (s f, Result a)
simpleWith r s x = runFabT x >>= simpleResult r s

simpleResult :: (Monad f, HasFabStore s f) => Recorder s f -> s f -> FabResult f a -> f (s f, Result a)
simpleResult _ s (Value a)       = pure (s, Pure a)
simpleResult _ s (Error a)       = pure (s, Throw a)
simpleResult r s (Request req c) = simpleResolve r s c req

-- FIXME It seems like the rest of these could be... simplier, or at least less verbose.
simpleResolve :: (Monad f, HasFabStore s f) => Recorder s f -> s f -> (b -> FabT f a) -> FabRequest f k b -> f (s f, Result a)
simpleResolve r s c = \case
       ForConfig -> simpleWith r s $ pure (getConfig s) >>= c
       ForCachedKey k -> do
         (s', v) <- simpleFetch s k
         s'' <- case sequence v of
                     Nothing -> pure s'
                     Just v' -> r k v' <&> ($ s')
         case v of
              Throw e -> pure (s'', Throw e)
              Pure x  -> simpleWith r s'' $ c x
       ForKey k -> do
         (s', v) <- simpleBuild s k
         s'' <- r k v <&> ($ s')
         case v of
              Throw e -> pure (s'', Throw e)
              Pure x  -> simpleWith r s'' $ c x
       ForBoth a b -> do
         (s', a') <- simpleResolve r s pure a
         (s'', b') <- simpleResolve r s' pure b
         case (a', b') of
              (Pure x, Pure y) -> simpleWith r s'' $ c (x, y)
              (Throw e, _)     -> pure (s'', Throw e)
              (_, Throw e)     -> pure (s'', Throw e)

simpleFetch :: (Monad f, HasFabStore s f, Fab f k) => s f -> k -> f (s f, Result (Maybe (FabVal f k)))
simpleFetch s k = maybe (pure (s, pure Nothing)) check $ getValue k s
  where
    check v = do
      simpleWith norec s (verify k v (getRefab k s)) >>= \case
        (s', Pure True) -> pure (s', Just <$> v)
        (s', _)         -> pure (s', pure Nothing)

simpleBuild :: (Monad f, HasFabStore s f, Fab f k) => s f -> k -> f (s f, Result (FabVal f k))
simpleBuild s k = maybe (run s) check $ getValue k s
  where
    run s' = do
      (s'', v) <- simpleWith norec s' $ fabricate k
      fin <- finalize k v
      pure (putValue k v $ modifyRefab k fin s'', v)
    check v = do
      simpleWith norec s (verify k v (getRefab k s)) >>= \case
        (s', Pure True) -> pure (s', v)
        (s', _)         -> run s'

-- | Always rebuilds everything every time.
busy :: Monad f => FabT f a -> f (Result a)
busy = fmap snd <$> simple Proxy
