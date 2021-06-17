{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Trace Verifying Refabber(s).
-}

module Fab.Refab.VerifyTrace
  ( VerifyTrace
  ) where

import           Data.Default (Default)
import           Fab.Core (Fab, FabVal, Refabber, finalize, record, verify)
import           Fab.Trace (Hash, Trace(..), TracePair(..), mkHash)

-- | Verifying Traces record the hashes of the immediate dependencies and that
-- of our current build result. When it is time to rebuild, we then check all
-- these hashes against the hashes of their respective values in the store - if
-- all hashes match we don't need to rebuild.
--
-- This allows us to skip rebuilding if nothing has changed for this key since
-- the last build, but we drop older traces than that.
newtype VerifyTrace v f = VT (Trace (Maybe (Hash v)) f)
  deriving (Default, Show)

instance (Fab k f, v ~ FabVal k f) => Refabber (VerifyTrace v f) k f where
  finalize _ v = pure $ \(VT t) -> VT t { traceVal = Just $ mkHash v }
  record _ k' v' = pure $ \(VT t) -> VT t { traceDeps = TracePair k' (mkHash v') : traceDeps t }
  verify f _ v (VT Trace{..})
    | Just (mkHash v) /= traceVal = pure False
    | otherwise = all id <$> traverse checkTracePair traceDeps
        where checkTracePair (TracePair k hv) = (hv ==) . mkHash <$> f k
