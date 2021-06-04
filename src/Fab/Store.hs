{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Storing fabrication products and rebuild information.
-}

module Fab.Store
  ( Storable
    -- * Helpers for implementing 'Fab.Core.HasFabStore'.
  , FabStore
  , fabGetValue
  , fabPutValue
  , fabGetInfo
  , fabPutInfo
  , fabGetConfig
  , fabPutConfig
    -- * Insertions and lookups for one type of key, for lower level implementations of 'Fab.Core.HasFabStore'.
  , KeyStore
  , keyPutValue
  , keyGetValue
  , keyGetInfo
  , keyPutInfo
  ) where

import           Data.Default (Default(def))
import           Data.Dynamic (Dynamic, fromDynamic, toDyn)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Data.Typeable (Proxy(Proxy), TypeRep, Typeable, cast, typeOf, typeRep)
import           Fab.Core (Fab(FabVal, Refab))

-- | What something needs in order to be stored in a 'FabStore'.
type Storable k f = (Fab k f, Typeable f)

-- | A fabrication store for a particular type of key.
data KeyStore k f = KeyStore
   { keyInfoMap :: !(HashMap k (Refab k f))
   , keyValMap  :: !(HashMap k (FabVal k f))
   }

deriving instance Fab k f => Show (KeyStore k f)

instance Storable k f => Default (KeyStore k f) where
  def = KeyStore mempty mempty

-- | Update the value for a key in a 'KeyStore'.
keyPutValue :: Storable k f => k -> FabVal k f -> KeyStore k f -> KeyStore k f
keyPutValue k v ks = ks { keyValMap = HM.insert k v $ keyValMap ks }

-- | Get the value for a key from a 'KeyStore'.
keyGetValue :: Storable k f => k -> KeyStore k f -> Maybe (FabVal k f)
keyGetValue k = HM.lookup k . keyValMap

-- | Update the rebuild info for a key in a 'KeyStore'.
keyPutInfo :: Storable k f => k -> Refab k f -> KeyStore k f -> KeyStore k f
keyPutInfo k i ks = ks { keyInfoMap = HM.insert k i $ keyInfoMap ks }

-- | Get the rebuild info for a key from a 'KeyStore'.
keyGetInfo :: Storable k f => k -> KeyStore k f -> Refab k f
keyGetInfo k = fromMaybe def . HM.lookup k . keyInfoMap

-- | An existential wrapper around some 'KeyStore'.
data SubStore (f :: * -> *) = forall k. Storable k f => SubStore !(KeyStore k f)

deriving instance Show (SubStore f)

-- | A fabrication store, which tracks fabrication values and rebuild
-- information for any fabrication key.
data FabStore f = FabStore (HashMap TypeRep (SubStore f)) (HashMap TypeRep Dynamic)
  deriving (Show)

instance Default (FabStore f) where
  def = FabStore mempty mempty

-- | Extract the 'KeyStore' for a particular (type of) key from a 'FabStore f'.
getKeyStore :: forall k f. Storable k f => k -> FabStore f -> KeyStore k f
getKeyStore k (FabStore hm _) = fromMaybe def $ do
  SubStore ss <- HM.lookup (typeOf k) hm
  cast ss

-- | Update the 'KeyStore' for a particular (type of) key in a 'FabStore f'.
putKeyStore :: forall k f. Storable k f => k -> KeyStore k f -> FabStore f -> FabStore f
putKeyStore k ks (FabStore hm cm) = FabStore (HM.insert (typeOf k) (SubStore ks) hm) cm

-- | Apply a function to the 'KeyStore' for a particular (type of) key in a 'FabStore f'.
overKeyStore :: forall k f. Storable k f => k -> (KeyStore k f -> KeyStore k f) -> FabStore f -> FabStore f
overKeyStore k f s = putKeyStore k (f $ getKeyStore k s) s

-- | Fetch a value for a key from a 'FabStore f'.
fabGetValue :: Storable k f => k -> FabStore f -> Maybe (FabVal k f)
fabGetValue k = keyGetValue k . getKeyStore k

-- | Update a value for a key in a 'FabStore f'.
fabPutValue :: Storable k f => k -> FabVal k f -> FabStore f -> FabStore f
fabPutValue k v = overKeyStore k (keyPutValue k v)

-- | Fetch rebuild information for a (type of) key from a 'FabStore f'.
fabGetInfo :: Storable k f => k -> FabStore f -> Refab k f
fabGetInfo k = keyGetInfo k . getKeyStore k

-- | Update rebuild information for a (type of) key in a 'FabStore f'.
fabPutInfo :: Storable k f => k -> Refab k f -> FabStore f -> FabStore f
fabPutInfo k = overKeyStore k . keyPutInfo k

-- | Get some configuration data from the store.
fabGetConfig :: forall c f. (Default c, Typeable c) => FabStore f -> c
fabGetConfig (FabStore _ cm) = fromMaybe def $ fromDynamic =<< HM.lookup (typeRep (Proxy @c)) cm

-- | Put some configuration data into the store.
fabPutConfig :: forall c f. (Typeable c) => c -> FabStore f -> FabStore f
fabPutConfig c (FabStore hm cm) = FabStore hm $ HM.insert (typeOf c) (toDyn c) cm
