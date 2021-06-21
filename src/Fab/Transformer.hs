{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Fabrication Monad Transformer
-}

module Fab.Transformer
  ( FabT(..)
  , liftCatch
  ) where

import           Control.Applicative (Alternative, empty, liftA2, (<|>))
import           Control.Monad (MonadPlus)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Signatures (Catch)
import           Control.Monad.State.Class (MonadState, get, gets, modify, put)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Bifunctor (first)
import           Data.Typeable (Typeable)

import           Fab.Core (HasFabStore, getConfig, getRefab, getValue, putConfig, putRefab,
                     putValue)
import           Fab.Store (FabStore, fabGetConfig, fabGetInfo, fabGetValue, fabPutConfig,
                     fabPutInfo, fabPutValue)

-- | Fabrication transformer that adds 'HasFabStore' to another 'Monad'.
newtype FabT m a = FabT { runFabT :: FabStore (FabT m) -> m (a, FabStore (FabT m)) }

-- | Lift a catch operation (a mirror of '>>=') through 'FabT'.
liftCatch :: Catch e m (a, FabStore (FabT m)) -> Catch e (FabT m) a
liftCatch catch m handler = FabT $ \st -> catch (runFabT m st) (\e -> runFabT (handler e) st)

instance Functor m => Functor (FabT m) where
  fmap f x = FabT $ \st -> fmap (first f) $ runFabT x st

instance Monad m => Applicative (FabT m) where
  pure a = FabT $ pure . (a,)
  ff <*> fx = FabT $ \st -> do
    (f, st') <- runFabT ff st
    first f <$> runFabT fx st'
  liftA2 f fa fb = FabT $ \st -> do
    (a, st') <- runFabT fa st
    first (f a) <$> runFabT fb st'
  fa *> fb = FabT $ \st -> snd <$> runFabT fa st >>= runFabT fb
  fa <* fb = FabT $ \st -> do
    (a, st') <- runFabT fa st
    first (const a) <$> runFabT fb st'

instance (Alternative m, Monad m) => Alternative (FabT m) where
  empty = lift empty
  fa <|> fb = FabT $ \st -> runFabT fa st <|> runFabT fb st

instance MonadPlus m => MonadPlus (FabT m)
instance MonadFail m => MonadFail (FabT m) where
  fail = lift . fail

instance Monad m => Monad (FabT m) where
  m >>= f = FabT $ \st -> do
    (x, st') <- runFabT m st
    runFabT (f x) st'

instance MonadTrans FabT where
  lift m = FabT $ \st -> (,st) <$> m

instance MonadIO m => MonadIO (FabT m) where
  liftIO = lift . liftIO

instance Monad m => MonadState (FabStore (FabT m)) (FabT m) where
  get = FabT $ \st -> pure (st,st)
  put st = FabT $ \_ -> pure ((),st)

instance (Monad m, Typeable m) => HasFabStore (FabT m) where
  getValue k = gets (fabGetValue k)
  getRefab k = gets (fabGetInfo k)
  putValue k v = modify (fabPutValue k v)
  putRefab k i = modify (fabPutInfo k i)
  getConfig = gets (fabGetConfig)
  putConfig = modify . fabPutConfig
