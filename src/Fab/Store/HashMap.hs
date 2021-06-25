{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Storing fabrication products and rebuild information.
-}

module Fab.Store.HashMap
  ( Storable
    -- * Helpers for implementing 'Fab.Core.HasFabStore'.
  , FabStore
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
import           Fab.Core (Fab(FabVal, Validation))
import           Fab.Result (Result)
import           Fab.Store (HasFabStore, getConfig, getValidation, getValue, putConfig,
                     putValidation, putValue)

-- | What something needs in order to be stored in a 'FabStore'.
type Storable f k = (Fab f k, Typeable f)

-- | A fabrication store for a particular type of key.
data KeyStore f k = KeyStore
   { keyInfoMap :: !(HashMap k (Validation f k))
   , keyValMap  :: !(HashMap k (Result (FabVal f k)))
   }

deriving instance Fab f k => Show (KeyStore f k)

instance Storable f k => Default (KeyStore f k) where
  def = KeyStore mempty mempty

-- | Update the value for a key in a 'KeyStore'.
keyPutValue :: Storable f k => k -> Result (FabVal f k) -> KeyStore f k -> KeyStore f k
keyPutValue k v ks = ks { keyValMap = HM.insert k v $ keyValMap ks }

-- | Get the value for a key from a 'KeyStore'.
keyGetValue :: Storable f k => k -> KeyStore f k -> Maybe (Result (FabVal f k))
keyGetValue k = HM.lookup k . keyValMap

-- | Update the rebuild info for a key in a 'KeyStore'.
keyPutInfo :: Storable f k => k -> Validation f k -> KeyStore f k -> KeyStore f k
keyPutInfo k i ks = ks { keyInfoMap = HM.insert k i $ keyInfoMap ks }

-- | Get the rebuild info for a key from a 'KeyStore'.
keyGetInfo :: Storable f k => k -> KeyStore f k -> Validation f k
keyGetInfo k = fromMaybe def . HM.lookup k . keyInfoMap

-- | An existential wrapper around some 'KeyStore'.
data SubStore (f :: * -> *) = forall k. Storable f k => SubStore !(KeyStore f k)

deriving instance Show (SubStore f)

-- | A fabrication store, which tracks fabrication values and rebuild
-- information for any fabrication key.
data FabStore f = FabStore (HashMap TypeRep (SubStore f)) (HashMap TypeRep Dynamic)
  deriving (Show)

instance Default (FabStore f) where
  def = FabStore mempty mempty

-- | Extract the 'KeyStore' for a particular (type of) key from a 'FabStore f'.
getKeyStore :: forall f k. Storable f k => k -> FabStore f -> KeyStore f k
getKeyStore k (FabStore hm _) = fromMaybe def $ do
  SubStore ss <- HM.lookup (typeOf k) hm
  cast ss

-- | Update the 'KeyStore' for a particular (type of) key in a 'FabStore f'.
putKeyStore :: forall f k. Storable f k => k -> KeyStore f k -> FabStore f -> FabStore f
putKeyStore k ks (FabStore hm cm) = FabStore (HM.insert (typeOf k) (SubStore ks) hm) cm

-- | Apply a function to the 'KeyStore' for a particular (type of) key in a 'FabStore f'.
overKeyStore :: forall f k. Storable f k => k -> (KeyStore f k -> KeyStore f k) -> FabStore f -> FabStore f
overKeyStore k f s = putKeyStore k (f $ getKeyStore k s) s

instance Typeable f => HasFabStore FabStore f where
  getValue k = keyGetValue k . getKeyStore k
  putValue k v = overKeyStore k (keyPutValue k v)
  getValidation k = keyGetInfo k . getKeyStore k
  putValidation k = overKeyStore k . keyPutInfo k
  getConfig :: forall c. (Default c, Typeable c) => FabStore f -> c
  getConfig (FabStore _ cm) = fromMaybe def $ fromDynamic =<< HM.lookup (typeRep (Proxy @c)) cm
  putConfig c (FabStore hm cm) = FabStore hm $ HM.insert (typeOf c) (toDyn c) cm
