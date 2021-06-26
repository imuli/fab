{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Trace Verifying Validator(s).
-}

module Fab.Validator.VerifyTrace
  ( VerifyTrace
  ) where

import           Data.Default (Default, def)
import           Fab.Core
import           Fab.Result (Result, toResult)
import           Fab.Trace (Hash, Trace(..), TracePair(..), mkHash)

-- | Verifying Traces record the hashes of the immediate dependencies and that
-- of our current build result. When it is time to rebuild, we then check all
-- these hashes against the hashes of their respective values in the store - if
-- all hashes match we don't need to rebuild.
--
-- This allows us to skip rebuilding if nothing has changed for this key since
-- the last build, but we drop older traces than that.
newtype VerifyTrace f v = VT (Trace f (Maybe (Hash (Result v))))
  deriving (Default, Show)

instance (Fab f k, v ~ FabVal f k) => Validator f k (VerifyTrace f v) where
  finalize _ v = pure $ \(VT t) -> VT t { traceVal = Just $ mkHash v }
  record _ k' v' = pure $ \(VT t) -> if null $ traceVal t
                                        then VT t { traceDeps = TracePair k' (mkHash v') : traceDeps t }
                                        else VT def { traceDeps = [TracePair k' (mkHash v')] }
  verify _ v (VT Trace{..})
    | Just (mkHash v) /= traceVal = pure False
    | otherwise = all id <$> traverse checkTracePair traceDeps
        where checkTracePair (TracePair k hv) = (hv ==) . mkHash <$> toResult (fab @f k)
