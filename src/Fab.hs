{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Caching Refabber(s).
-}

module Fab
  ( Fab(..)
    -- * Implementing Fab
  , Scheduler
  , Fabber
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
  , simpleFab
  , logFab
    -- * Store
  , FabT
  , runFabT
  , HasFabStore(..)
  , configure
  , getExistingValue
    -- * Existential Wrappers
  , FabKey(..)
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
import           Fab.Scheduler
import           Fab.Transformer
