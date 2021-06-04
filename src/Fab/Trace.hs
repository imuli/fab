{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Fab.Trace
  ( Hash
  , mkHash
  , TracePair(..)
  , Trace(..)
  ) where

import           Data.Default (Default, def)
import           Data.Hashable (Hashable(hash))
import           Fab.Core (Fab, FabVal)

-- | Replace with a real hash.
newtype Hash a = Hash Int
  deriving (Eq, Ord, Show)

mkHash :: Hashable a => a -> Hash a
mkHash = Hash . hash

data TracePair f = forall k. Fab k f => TracePair !k !(Hash (FabVal k f))

deriving instance Show (TracePair f)

data Trace v f = Trace
   { traceDeps :: [TracePair f]
   , traceVal  :: v
   }
  deriving (Show)

instance Default v => Default (Trace v f) where
  def = Trace def def
