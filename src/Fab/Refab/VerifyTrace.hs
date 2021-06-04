{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Trace Verifying Refabber(s).
-}

module Fab.Refab.VerifyTrace
  ( VerifyTrace
  ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Data.Bool (bool)
import           Data.Default (Default)
import           Data.Hashable (Hashable)
import           Fab.Core (Fab, FabVal, Refabber, Scheduler, fab, getRefab, getValue, modifyRefab,
                     putRefab, refab, updateValue)
import           Fab.Trace (Hash, Trace(..), TracePair(..), mkHash)

-- | Verifying Traces record the hashes of the immediate dependencies and that
-- of our current build result. When it is time to rebuild, we then check all
-- these hashes against the hashes of their respective values in the store - if
-- all hashes match we don't need to rebuild.
--
-- This allows us to skip rebuilding if nothing has changed for this key since
-- the last build, but there's no point in keeping older traces than that.
newtype VerifyTrace v f = VT (Trace (Maybe (Hash v)) f)
  deriving (Default, Show)

instance (Fab k f, v ~ FabVal k f, Alternative f) => Refabber (VerifyTrace v f) k f where
  refab f k = traceVerified <|> refabricate
   where
    traceVerified = do
      vt <- getRefab k
      val <- maybe empty pure =<< getValue k
      verifyTrace f val vt
    refabricate = do
      v <- fab record k
      VT t <- getRefab k
      putRefab k (VT $ t { traceVal = Just $ mkHash v })
      updateValue k v
    record :: forall t. Fab t f => t -> f (FabVal t f)
    record k' = do
      v' <- f k'
      modifyRefab k (recordDependancy k' v')
      pure v'

verifyTrace :: (Alternative f, Monad f, Hashable v) => Scheduler f -> v -> VerifyTrace v f -> f v
verifyTrace f v (VT Trace{..}) | Just (mkHash v) /= traceVal = empty
                               | otherwise = bool (pure v) empty =<< all id <$> traverse (\(TracePair k hv) -> (hv ==) . mkHash <$> f k) traceDeps

recordDependancy :: Fab k f => k -> FabVal k f -> VerifyTrace v f -> VerifyTrace v f
recordDependancy k v (VT t) = VT t { traceDeps = TracePair k (mkHash v) : traceDeps t }
