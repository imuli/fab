{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Caching Refabber(s).
-}

module Fab
  ( Fab(..)
  , FabT
  , Result(..)
    -- * Implementing Fab
  , FabKey
  , FabValue
  , fab
  , cachedFab
  , config
  , throw
    -- * Refabbers
  , Refabber(..)
    -- ** Simple
  , Avoid
    -- ** Caching
  , Cache
  , CacheTimeout
    -- ** Tracing
  , VerifyTrace
    -- * Schedulers
  , busy
  , simple
    -- * Store
  , HasFabStore(..)
  , configure
  , FabStore
    -- * Existential Wrappers
  , SomeFabKey(..)
  , HoldsFabKey(..)
  , FabPair(..)
  , HoldsFabPair(..)
  , putFabPair
  ) where

import           Fab.Core
import           Fab.Existential
import           Fab.Refab.Avoid
import           Fab.Refab.Cache
import           Fab.Refab.VerifyTrace
import           Fab.Result
import           Fab.Scheduler
import           Fab.Store
import           Fab.Store.HashMap
