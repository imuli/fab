{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fab.Existential where

import           Fab.Core

-- | Wrap up a fabrincation key with it's 'Fab' instance.
data FabKey f = forall k. Fab k f => FabKey k

-- | Extract some fabrication key from somehing.
class HoldsFabKey t f where
  toFabKey :: t -> FabKey f

-- | Wrap up a fabrincation key-value pair with their 'Fab' instance.
data FabPair f = forall k. Fab k f => FabPair k (FabVal k f)

-- | Extract a fabrication key-value pair from something.
class HoldsFabPair t f where
  toFabPair :: t -> FabPair f
