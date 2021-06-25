{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : The core types and type classes for Fab.
-}

module Fab.Core
  ( FabKey
  , FabValue
  , Fab(..)
  , fab
  , cachedFab
  , config
  , FabT(..)
  , runFabT
  , throw
  , FabResult(..)
  , FabRequest(..)
  , Refabber(..)
  ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Exception (Exception, SomeException, fromException, toException)
import           Control.Monad ((>=>))
import           Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Default (Default)
import           Data.Hashable (Hashable)
import           Data.Typeable (Typeable)
import           Fab.Result (Result, ResultException(Empty, Fail))
import           GHC.Generics (Generic)

-- | Type helper to constrain your fabrication key (or segments there of).
type FabKey k = (Show k, Typeable k, Eq k, Hashable k)

-- | Type helper to constrain your fabrication values (or segments there of).
type FabValue v = (Show v, Typeable v, Hashable v)

-- | Fabricating a key in some 'Functor'.
--
-- You can't actually request fabrication of anything weaker than an
-- 'Applicative', and can't run it in anything weaker than a 'Monad'...
class (FabKey k, Refabber f k (Refab f k), FabValue (FabVal f k)) => Fab f k where

  -- | The information required to test whether we need to rebuild the key.
  type family Refab f k
  type instance Refab f k = ()

  -- | What sort of values does this key produce?
  type family FabVal f k

  -- | Actually do the fabrication.
  --
  -- This should not call other versions of itself (unless you always want to
  -- recreate a particular subgoal). Instead it should call 'fab'.
  fabricate :: k -> FabT f (FabVal f k)

-- | Refabbers implement this typeclass, so that to change refabbers for a
-- particular key you only need to change 'Refab'.
--
-- The general ideal is that we're given keys and 'Result's at various hooks in
-- the process, which lets us record information about the build process. This
-- is heavily designed for 'Fab.Trace.Trace'-derived 'Refabber's and may change
-- if other types have more needs.
--
-- The defaults implementation keeps no information and never 'verify's.
class (Show i, Default i) => Refabber f k i where

  -- | Update fabrication information given a final result.
  --
  -- This has type @f (i -> i)@ to allow queries to @f@, for example to allow
  -- the 'Fab.Refab.Cache.Cache' refabber to check the time.
  finalize :: (Refab f k ~ i, Applicative f) => k -> Result (FabVal f k) -> f (i -> i)
  finalize _ _ = pure id

  -- | Update fabrication information given a dependancy.
  --
  -- This has type @f (i -> i)@ to allow queries to @f@, mirroring finalize.
  record :: (Refab f k ~ i, Fab f t, Applicative f) => k -> t -> Result (FabVal f t) -> f (i -> i)
  record _ _ _ = pure id

  -- | Verify a key-value-info set.
  --
  -- This may call the scheduler. 'Fab.Refab.VerifyTrace.VerifyTrace', for
  -- example, will check to make sure all the immediate dependencies are up to
  -- date.
  verify :: (Refab f k ~ i, Monad f) => k -> Result (FabVal f k) -> i -> FabT f Bool
  verify _ _ _ = pure False

instance Refabber f k ()

-- | Request something from the Scheduler.
data FabRequest f k a where
   -- | Request a configuration value.
   ForConfig :: (Typeable k, Default k) => FabRequest f k k
   -- | Request a that a key be fabricated.
   ForKey :: Fab f k => k -> FabRequest f k (FabVal f k)
   -- | Request a key from the store, but don't rebuild it if it's stale.
   ForCachedKey :: Fab f k => k -> FabRequest f k (Maybe (FabVal f k))
   -- | Request multiple things.
   ForBoth :: FabRequest f k a -> FabRequest f k' a' -> FabRequest f (k, k') (a, a')

deriving instance Show (FabRequest f k a)

-- | Request that the scheduler fabricate this key.
fab :: (Applicative f, Fab f k) => k -> FabT f (FabVal f k)
fab k = FabT . pure $ Request (ForKey k) pure

-- | Request the cached value for this key, if it's valid.
cachedFab :: (Applicative f, Fab f k) => k -> FabT f (Maybe (FabVal f k))
cachedFab k = FabT . pure $ Request (ForCachedKey k) pure

-- | Request a config entry (of a particular type)
config :: (Applicative f, Typeable a, Default a) => FabT f a
config = FabT . pure $ Request ForConfig pure

-- | The core fabrication 'Monad'.
--
-- This leans heavily on the 'FabResult' Monad.
newtype FabT f a = FabT (f (FabResult f a))
  deriving (Generic)

deriving instance (forall t. Show t => Show (f t), Show a) => Show (FabT f a)

-- | Compute as much of a 'FabT' as we can without outside help.
runFabT :: FabT f a -> f (FabResult f a)
runFabT (FabT a) = a

instance Functor f => Functor (FabT f) where
  fmap f mx = FabT $ fmap f <$> runFabT mx

instance Applicative f => Applicative (FabT f) where
  pure = FabT . pure . pure
  ff <*> fa = FabT $ do
    f <- runFabT ff
    a <- runFabT fa
    pure $ f <*> a

instance Monad f => Monad (FabT f) where
  fa >>= f = FabT $ do
    a <- runFabT fa
    case a of
         Value x     -> (runFabT <$> f) x
         Error e     -> pure $ Error e
         Request r c -> pure $ Request r $ c >=> f

-- | The intermediate result 'Monad' for fabrication.
--
-- This allows us to collect requests until they are required.
data FabResult f a
   = Value a
   | Error SomeException
   | forall k b. Request (FabRequest f k b) (b -> FabT f a)

instance Show a => Show (FabResult f a) where
  showsPrec prec (Value a)     = showParen (prec > 10) $ showString "Value " . showsPrec 11 a
  showsPrec prec (Error e)     = showParen (prec > 10) $ showString "Error " . showsPrec 11 e
  showsPrec prec (Request r _) = showParen (prec > 10) $ showString "Request " . showsPrec 11 r

instance Functor f => Functor (FabResult f) where
  fmap f (Value a)     = Value $ f a
  fmap _ (Error e)     = Error e
  fmap f (Request r c) = Request r $ fmap f <$> c

instance Applicative f => Applicative (FabResult f) where
  pure = Value
  Value f <*> fa                = f <$> fa
  Error e <*> _                 = Error e
  Request r c <*> Value a       = Request r $ fmap ($ a) <$> c
  Request r _ <*> Error e       = Request r $ \_ -> throw e
  Request r c <*> Request r' c' = Request (ForBoth r r') (\(b,b') -> c b <*> c' b')

instance Monad f => Monad (FabResult f) where
  Value a >>= f     = f a
  Error e >>= _     = Error e
  Request r c >>= f = Request r $ c >=> FabT . pure . f

instance Monad f => Alternative (FabResult f) where
  empty = throwM Empty
  a <|> b = a `catch` (\(_ :: SomeException) -> b)

instance MonadFail f => MonadFail (FabResult f) where
  fail = throwM . Fail

instance Monad f => MonadThrow (FabResult f) where
  throwM = Error . toException

instance Monad f => MonadCatch (FabResult f) where
  catch (Value a) _          = Value a
  catch (Error e) handle     = maybe (Error e) handle $ fromException e
  catch (Request r c) handle = Request r $ \b -> catch (c b) (FabT . pure . handle)

-- | Provided for convenience for anyone who wants to throw an error in an
-- applicative context.
throw :: (Exception e, Applicative f) => e -> FabT f a
throw = FabT . pure . Error . toException

instance Monad f => Alternative (FabT f) where
  empty = throw Empty
  a <|> b = a `catch` (\(_ :: SomeException) -> b)

instance MonadFail f => MonadFail (FabT f) where
  fail = throw . Fail

instance Monad f => MonadThrow (FabT f) where
  throwM = throw

instance Monad f => MonadCatch (FabT f) where
  catch fa handle = FabT $ do
    a <- runFabT fa
    case a of
         Value x     -> pure $ Value x
         Error e     -> maybe (pure $ Error e) (runFabT <$> handle) (fromException e)
         Request r c -> pure $ Request r $ \b -> catch (c b) handle

instance MonadIO f => MonadIO (FabT f) where
  liftIO f = FabT $ Value <$> liftIO f

instance MonadTrans FabT where
  lift f = FabT $ Value <$> f
