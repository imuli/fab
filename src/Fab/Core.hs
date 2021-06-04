{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : The core types and type classes for Fab.
-}

module Fab.Core
  ( Scheduler
  , Fabber
  , Fab(..)
  , Refabber(..)
  , HasFabStore(..)
  , configure
  , updateValue
  , modifyRefab
  ) where

import           Data.Default (Default(def))
import           Data.Functor.Const (Const(Const))
import           Data.Hashable (Hashable)
import           Data.Typeable (Typeable)

-- | For any fabrication key that can be fabricated in this 'Functor',
-- fabricate it's value.
type Scheduler f = forall k. Fab k f => k -> f (FabVal k f)

-- | Given a function to fabricate any key from it's value,
type Fabber k f = Scheduler f -> k -> f (FabVal k f)

-- | Fabricating a key in some 'Functor'.
class ( Show k, Eq k, Hashable k, Typeable k
      , Refabber (Refab k f) k f
      , Show (FabVal k f), Hashable (FabVal k f)
      ) => Fab k f where

  -- | The information required to test whether we need to rebuild the key.
  type family Refab k f
  type instance Refab k f = ()

  -- | What sort of values does this key produce?
  type family FabVal k f

  -- | Fabricate a specific key, given a function that can fabricate any other
  -- fabricatable key.
  fab :: Fabber k f

-- | Refabbers implement this typeclass, so that to change refabbers for a
-- particular key you only need to change 'Refab'.
class (Show i, Default i) => Refabber i k f where
  refab :: (Refab k f ~ i, HasFabStore f, Monad f) => Fabber k f

-- | Always fabricate the value, never store it.
instance (Fab k f) => Refabber () k f where
  refab = fab

-- | An appropriate 'Functor' can hold fabrication products as well as the
-- information required to attempt to rebuild them.
class HasFabStore f where
  -- | Fetch a fabrication product for a key, if it exists.
  getValue :: forall k. Fab k f => k -> f (Maybe (FabVal k f))

  -- | Update the stored fabrication product for a key.
  putValue :: forall k. Fab k f => k -> (FabVal k f) -> f ()

  -- | Fetch the rebuild information for a fabrication key.
  --
  -- Note that this information is always a `Monoid` and thus can be
  -- synthesized if needed.
  getRefab :: forall k. Fab k f => k -> f (Refab k f)

  -- | Update the rebuild information for a fabrication key.
  putRefab :: forall k. Fab k f => k -> (Refab k f) -> f ()

  -- | Fetch some configuration information of a particular type.
  --
  -- This is useful for things like a database backend (to carry connection
  -- information), or caching rebuilders (which rebuild after some configurable
  -- amount of time has passed).
  getConfig :: forall c. (Default c, Typeable c) => f c
  default getConfig :: Applicative f => forall c. (Default c) => f c
  getConfig = pure def

  -- | Replace configuration information of a particular type.
  --
  -- Typically this would only be called during setup.
  putConfig :: forall c. (Typeable c) => c -> f ()
  default putConfig :: Applicative f => forall c. c -> f ()
  putConfig _ = pure ()

-- | A small helper to put a value into a store and then return it.
updateValue :: (Fab k f, Applicative f, HasFabStore f) => k -> FabVal k f -> f (FabVal k f)
updateValue k v = putValue k v *> pure v

-- | Modify the refab information for a key with a function.
modifyRefab :: (Fab k f, Monad f, HasFabStore f) => k -> (Refab k f -> Refab k f) -> f ()
modifyRefab k f = putRefab k =<< f <$> getRefab k

-- | Add configuration information.
--
-- Note that this should usually only be done before fabricating any products -
-- most refabbers will not check whether any configuration information has
-- changed! And there is no easy method to intercept what configuration
-- information is used for fabrication.
configure :: (HasFabStore f, Typeable c) => (a -> c) -> a -> f ()
configure f x = putConfig (f x)

-- | The @Const m@ applicative functor cannot contain values, so it simply
-- discards everything. It can be used to calculate dependencies if the build
-- plan can be expressed in an applicative.
--
-- In essence, it always has an empty store.
instance Monoid m => HasFabStore (Const m) where
  getValue _ = Const mempty
  putValue _ _ = Const mempty
  getRefab _ = Const mempty
  putRefab _ _ = Const mempty

-- | The 'Maybe' monad has a perpetually empty store.
instance HasFabStore Maybe where
  getValue _ = Just Nothing
  putValue _ _ = Just ()
  getRefab _ = Just def
  putRefab _ _ = Just ()
