{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Existential types for Fab
-}

module Fab.Existential where

import           Data.Hashable (Hashable, hashWithSalt)
import           Data.Typeable (Typeable, cast, typeOf)
import           Fab.Core
import           Fab.Result (Result, toResult)
import           Fab.Store (HasFabStore, putValue)

-- | Wrap up a fabrication key with it's 'Fab' instance.
data SomeFabKey f = forall k. Fab f k => SomeFabKey k

deriving instance Show (SomeFabKey f)

instance Eq (SomeFabKey f) where
  SomeFabKey x == SomeFabKey y = maybe False (x ==) $ cast y

instance Hashable (SomeFabKey f) where
  hashWithSalt salt (SomeFabKey k) = hashWithSalt salt (typeOf k, k)

-- | Extract some fabrication key from somehing.
class HoldsFabKey f t where
  toFabKey :: t -> SomeFabKey f

-- | Wrap up a fabrication key-value pair with their 'Fab' instance.
data FabPair f = forall k. Fab f k => FabPair k (Result (FabVal f k))

deriving instance Show (FabPair f)

instance Hashable (FabPair f) where
  hashWithSalt salt (FabPair k v) = hashWithSalt salt (typeOf k, k, v)

-- | Extract a fabrication key-value pair from something.
class HoldsFabPair f t where
  toFabPair :: t -> FabPair f

instance HoldsFabPair f (FabPair f) where
  toFabPair = id

instance (FabVal f k ~ v, Fab f k) => HoldsFabPair f (k, Result v) where
  toFabPair (k,v) = FabPair k v

-- | By always rebuilding, this inherits the rebuild properties of whatever is wrapped in it.
instance (Typeable f, Monad f) => Fab f (SomeFabKey f) where
  type instance FabVal f (SomeFabKey f) = FabPair f
  fabricate (SomeFabKey k) = FabPair k <$> toResult (fab k)

-- | Add something that holds a fabrication key-value pair to a store.
putFabPair :: forall f t s. (HoldsFabPair f t, HasFabStore s f) => t -> s f -> s f
putFabPair x = case toFabPair @f x of
                    FabPair k v -> putValue k v
