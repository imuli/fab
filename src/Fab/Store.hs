{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : Fabrication stores and helpers.
-}

module Fab.Store
  ( HasFabStore(..)
  , modifyRefab
  , configure
  ) where

import           Data.Default (Default, def)
import           Data.Proxy (Proxy)
import           Data.Typeable (Typeable)
import           Fab.Core
import           Fab.Result (Result)

-- | A datastructure that holds configuration information, fabrication
-- products, and the information required to attempt to rebuild them.
class HasFabStore s f where
  -- | Fetch a fabrication product for a key, if it exists.
  getValue :: forall k. Fab f k => k -> s f -> Maybe (Result (FabVal f k))

  -- | Update the stored fabrication product for a key.
  putValue :: forall k. Fab f k => k -> Result (FabVal f k) -> s f -> s f

  -- | Fetch the rebuild information for a fabrication key.
  --
  -- Note that this information must have a `Default` instance and thus can be
  -- synthesized if needed.
  getRefab :: forall k. Fab f k => k -> s f -> Refab f k

  -- | Update the rebuild information for a fabrication key.
  putRefab :: forall k. Fab f k => k -> Refab f k -> s f -> s f

  -- | Fetch some configuration information of a particular type.
  --
  -- This is useful for things like a database backend (to carry connection
  -- information), or caching rebuilders (which rebuild after some configurable
  -- amount of time has passed).
  getConfig :: forall c. (Default c, Typeable c) => s f -> c
  default getConfig :: forall c. (Default c) => s f -> c
  getConfig = const def

  -- | Replace configuration information of a particular type.
  --
  -- Typically this would only be called during setup.
  putConfig :: forall c. (Typeable c) => c -> s f -> s f
  default putConfig :: forall c. c -> s f -> s f
  putConfig _ = id

instance HasFabStore Proxy f where
  getValue _ _ = Nothing
  putValue _ _ = id
  getRefab _ _ = def
  putRefab _ _ = id

-- | Modify the refab information for a key with a function.
modifyRefab :: (Fab f k, Monad f, HasFabStore s f) => k -> (Refab f k -> Refab f k) -> s f -> s f
modifyRefab k f s = putRefab k (f $ getRefab k s) s

-- | Add configuration information.
--
-- Note that this should usually only be done before fabricating any products -
-- most refabbers will not check whether any configuration information has
-- changed! And there is no easy method to intercept what configuration
-- information is used for fabrication.
configure :: (HasFabStore s f, Typeable c) => (a -> c) -> a -> s f -> s f
configure f x = putConfig (f x)
