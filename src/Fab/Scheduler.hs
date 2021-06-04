{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Schedulers for fabrication.
-}

module Fab.Scheduler
  ( busyFab
  , simpleFab
  , logFab
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Fab.Core (HasFabStore, Scheduler, fab, refab)

-- | Always rebuilds everything every time.
busyFab :: Scheduler f
busyFab = fab busyFab

-- | Simple fabrication using the refab helper.
simpleFab :: (Monad f, HasFabStore f) => Scheduler f
simpleFab = refab simpleFab

-- | Simple fabrication using the refab helper.
logFab :: forall f. (HasFabStore f, MonadIO f) => Scheduler f
logFab = go 1
  where
    go :: Int -> Scheduler f
    go n k = do
      liftIO $ putStrLn $ replicate n '»' <> " building " <> show k
      v <- refab (go $ n+1) k
      liftIO $ putStrLn $ replicate n '»' <> " finished " <> show k
      pure v
