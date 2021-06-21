{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Fabrication Monad Transformer
-}

module Fab.Transformer
  ( FabT
  , runFabT
  ) where

import           Control.Applicative (Alternative)
import           Control.Monad (MonadPlus)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State.Strict (MonadState, StateT, gets, modify, runStateT)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Typeable (Typeable)

import           Fab.Core (HasFabStore, getConfig, getRefab, getValue, putConfig, putRefab,
                     putValue)
import           Fab.Store (FabStore, fabGetConfig, fabGetInfo, fabGetValue, fabPutConfig,
                     fabPutInfo, fabPutValue)

-- | Fabrication transformer that adds 'HasFabStore' to another 'Monad'.
newtype FabT m a = FabT (StateT (FabStore (FabT m)) m a)
  deriving (Applicative, Functor, Monad)

instance MonadTrans FabT where
  lift f = FabT $ lift f
deriving instance MonadPlus m => Alternative (FabT m)
deriving instance MonadPlus m => MonadPlus (FabT m)
deriving instance MonadIO m => MonadIO (FabT m)

deriving instance Monad m => MonadState (FabStore (FabT m)) (FabT m)

-- | Run a fabrication transformer.
runFabT :: FabT m a -> FabStore (FabT m) -> m (a, FabStore (FabT m))
runFabT (FabT st) = runStateT st

instance (Monad m, Typeable m) => HasFabStore (FabT m) where
  getValue k = gets (fabGetValue k)
  getRefab k = gets (fabGetInfo k)
  putValue k v = modify (fabPutValue k v)
  putRefab k i = modify (fabPutInfo k i)
  getConfig = gets (fabGetConfig)
  putConfig = modify . fabPutConfig
