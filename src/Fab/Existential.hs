{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Existential types for Fab
-}

module Fab.Existential where

import           Fab.Core

-- | Wrap up a fabrincation key with it's 'Fab' instance.
data FabKey f = forall k. Fab k f => FabKey k

-- | Extract some fabrication key from somehing.
class HoldsFabKey t f where
  toFabKey :: t -> FabKey f

-- | Wrap up a fabrication key-value pair with their 'Fab' instance.
data FabPair f = forall k. Fab k f => FabPair k (FabVal k f)

-- | Extract a fabrication key-value pair from something.
class HoldsFabPair t f where
  toFabPair :: t -> FabPair f

instance HoldsFabPair (FabPair f) f where
  toFabPair = id

instance (FabVal k f ~ v, Fab k f) => HoldsFabPair (k, v) f where
  toFabPair (k,v) = FabPair k v

-- | Add something that holds a fabrication key-value pair to a store.
putFabPair :: forall t f. (HoldsFabPair t f, HasFabStore f) => t -> f ()
putFabPair x = case toFabPair @t @f x of
                    FabPair k v -> putValue k v
