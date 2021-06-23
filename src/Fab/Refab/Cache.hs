{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Caching Refabber(s).
-}

module Fab.Refab.Cache
  ( CacheTimeout(..)
  , Cache
  ) where

import           Control.Applicative (liftA2)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Default (Default, def)
import           Fab.Core (Refabber, config, finalize, verify)
import           GHC.Clock (getMonotonicTime)

-- | How long to cache entries for `Cache`, in seconds.
--
-- This can go into the configuration of a store to override the default of 5 minutes.
newtype CacheTimeout = CacheTimeout Double
  deriving (Eq, Fractional, Num, Ord, Read, Show)

-- | The default Cache Timeout is 5 minutes.
instance Default CacheTimeout where
  def = 5 * 60

-- | Refabricate based on how long it has been since the old value was fabricated.
newtype Cache = Cache CacheTimeout
  deriving (Show)

instance Default Cache where
  def = Cache (-1/0)

now :: MonadIO f => f CacheTimeout
now = CacheTimeout <$> liftIO getMonotonicTime

instance MonadIO f => Refabber f k Cache where
  finalize _ _ = const . Cache <$> now
  verify _ _ (Cache expires) = (expires >) <$> liftA2 (+) config now
