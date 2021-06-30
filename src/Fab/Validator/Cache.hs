{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Caching Validator(s).
-}

module Fab.Validator.Cache
  ( CacheTimeout(..)
  , Cache
  , CacheTime
  ) where

import           Control.Applicative (liftA2)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import           Data.Default (Default, def)
import           Data.Polytime (Mono, Span, TimeSource, now)
import           Fab.Core (Validator, config, finalize, verify)
import           Linear.Affine ((.+^))

-- | How long to cache entries for `Cache`, in seconds.
--
-- This can go into the configuration of a store to override the default of 5 minutes.
newtype CacheTimeout = CacheTimeout (Span Float)
  deriving (Eq, Fractional, Num, Ord, Read, Show)

fromCacheTimeout :: CacheTimeout -> Span Float
fromCacheTimeout (CacheTimeout x) = x

-- | The default Cache Timeout is 5 minutes.
instance Default CacheTimeout where
  def = 5 * 60

-- | Validate based on how long it has been since the old value was fabricated.
newtype Cache = Cache (Mono Float)
  deriving (Show)

-- | Typeclass wrapper to obviate the need for UndecidableInstances.
class TimeSource (Mono Float) f => CacheTime f

instance CacheTime IO
instance {-# OVERLAPPING #-} Monad m => CacheTime (ReaderT (Mono Float) m)
instance {-# OVERLAPPING #-} Monad m => CacheTime (Lazy.StateT (Mono Float) m)
instance {-# OVERLAPPING #-} Monad m => CacheTime (Strict.StateT (Mono Float) m)
instance {-# OVERLAPPABLE #-} (CacheTime m, MonadTrans t, Monad m) => CacheTime (t m)

instance Default Cache where
  def = Cache (-1/0)

instance CacheTime f => Validator f k Cache where
  finalize _ _ = const . Cache <$> now
  verify _ _ (Cache expires) = (expires >) <$> liftA2 (.+^) now (fromCacheTimeout <$> config)
