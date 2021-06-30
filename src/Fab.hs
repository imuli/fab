{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Fabrication systems with customizable invalidation rules and schedulers.
-}

module Fab
  ( Fab(..)
  , FabT
    -- * Handling the results of fabrications.
  , Result(..)
  , fromResult
  , toResult
    -- * Helpers for implementing the 'Fab' type class.
    -- ** Constraint helpers.
  , FabKey
  , FabValue
    -- ** Making requests of the 'Scheduler'.
  , fab
  , cachedFab
  , config
    -- ** Utilities for error handling.
  , throw
  , liftCatch
    -- * 'Validator's
    -- ** Simple
  , Avoid
    -- ** Caching
  , Cache
  , CacheTimeout
    -- ** Tracing
  , VerifyTrace
    -- * 'Scheduler's
  , Scheduler
  , busy
  , simple
  , verbose
    -- * Store
  , HasFabStore(..)
  , FabStore
  , configure
    -- * Existential Wrappers
  , SomeFabKey(..)
  , HoldsFabKey(..)
  , FabPair(..)
  , HoldsFabPair(..)
  , putFabPair
  ) where

import           Fab.Core
import           Fab.Existential
import           Fab.Result
import           Fab.Scheduler
import           Fab.Store
import           Fab.Store.HashMap
import           Fab.Validator.Avoid
import           Fab.Validator.Cache
import           Fab.Validator.VerifyTrace
