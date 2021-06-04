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

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Default (Default, def)
import           Fab.Core (Fab, Refabber, fab, getConfig, getRefab, getValue, putRefab, refab,
                     updateValue)
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

instance (Fab k f, MonadIO f) => Refabber Cache k f where
  refab f k = maybe record verify =<< getValue k
    where
      getTime = CacheTimeout <$> liftIO getMonotonicTime
      record = do
        v <- fab f k
        t <- getTime
        putRefab k (Cache t)
        updateValue k v
      verify v = do
        Cache ft <- getRefab k
        t <- getTime
        ct <- getConfig
        if t > ft + ct then record
                       else pure v
