{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : The core types and type classes for Fab.

This module is for **internal** use, and exports may change without warning.
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
  , liftCatch
  , FabResult(..)
  , FabRequest(..)
  , Request(..)
  , Validator(..)
  ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Exception (Exception, SomeException, fromException, toException)
import           Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Default (Default)
import           Data.Hashable (Hashable)
import           Data.Typeable (Typeable)
import           Fab.Result (Result(Pure, Throw), ResultException(Empty, Fail), throwResult,
                     toResult)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

-- | Type helper to constrain your fabrication key (or segments there of).
type FabKey k = (Show k, Typeable k, Eq k, Hashable k)

-- | Type helper to constrain your fabrication values (or segments there of).
type FabValue v = (Show v, Typeable v, Hashable v)

-- | Fabricating a key in some 'Functor'.
--
-- You can't actually request fabrication of anything weaker than an
-- 'Applicative', and can't run it in anything weaker than a 'Monad'...
class (FabKey k, Validator f k (Validation f k), FabValue (FabVal f k)) => Fab f k where

  -- | The information required to test whether we need to rebuild the key.
  type family Validation f k
  type instance Validation f k = ()

  -- | What sort of values does this key produce?
  type family FabVal f k

  -- | Actually do the fabrication.
  --
  -- This should not call other versions of itself (unless you always want to
  -- recreate a particular subgoal). Instead it should call 'fab'.
  fabricate :: k -> FabT f (FabVal f k)

-- | Validators implement this typeclass, so that to change validation methods
-- for a particular key you only need to change 'Validation'.
--
-- The general ideal is that we're given keys and 'Result's at various hooks in
-- the process, which lets us record information about the build process. This
-- is heavily designed for 'Fab.Trace.Trace'-derived 'Validator's and may change
-- if other types have more needs.
--
-- The defaults implementation keeps no information and never 'verify's.
class (Show i, Default i) => Validator f k i where

  -- | Update fabrication information given a final result.
  --
  -- This has type @f (i -> i)@ to allow queries to @f@, for example to allow
  -- the 'Fab.Validator.Cache.Cache' validator to check the time.
  finalize :: (Validation f k ~ i, Applicative f) => k -> Result (FabVal f k) -> f (i -> i)
  finalize _ _ = pure id

  -- | Update fabrication information given a dependancy.
  --
  -- This has type @f (i -> i)@ to allow queries to @f@, mirroring finalize.
  record :: (Validation f k ~ i, Fab f t, Applicative f) => k -> t -> Result (FabVal f t) -> f (i -> i)
  record _ _ _ = pure id

  -- | Verify a key-value-info set.
  --
  -- This may call the scheduler. 'Fab.Validator.VerifyTrace.VerifyTrace', for
  -- example, will check to make sure all the immediate dependencies are up to
  -- date.
  verify :: (Validation f k ~ i, Monad f) => k -> Result (FabVal f k) -> i -> FabT f Bool
  verify _ _ _ = pure False

instance Validator f k ()

-- | The various things can can be requested from the 'Fab.Scheduler.Scheduler'.
data FabRequest f a where
   -- | Request a configuration value.
   ForConfig :: (Typeable k, Default k) => FabRequest f k
   -- | Request a that a key be fabricated.
   ForKey :: Fab f k => k -> FabRequest f (FabVal f k)
   -- | Request a key from the store, but don't rebuild it if it's stale.
   ForCachedKey :: Fab f k => k -> FabRequest f (Maybe (FabVal f k))

deriving instance Show (FabRequest f a)

-- | Request that the scheduler fabricate this key.
fab :: (Applicative f, Fab f k) => k -> FabT f (FabVal f k)
fab k = FabT . pure . Req $ Request (ForKey k) (throwResult throw)

-- | Request the cached value for this key, if it's valid.
cachedFab :: (Applicative f, Fab f k) => k -> FabT f (Maybe (FabVal f k))
cachedFab k = FabT . pure . Req $ Request (ForCachedKey k) (throwResult throw)

-- | Request a config entry (of a particular type)
config :: (Applicative f, Typeable a, Default a) => FabT f a
config = FabT . pure . Req $ Request ForConfig (throwResult throw)

-- | A non-empty tree of 'FabRequest's, with continuations to stitch the computation back together.
data Request f a
   -- | A single request.
   = forall b. Request (FabRequest f b) (Result b -> FabT f a)
   -- | An applicative composite of requests.
   | forall b c. ReqApp (Request f (c -> b)) (Request f c) (Result b -> FabT f a)

-- | Compose with the continuation in a 'Request'.
--
-- This is used in implementing 'Functor', 'Applicative', 'Monad', 'MonadCatch'
-- for 'FabT' and 'FabResult'.
reqComposeFabT :: Request f a -> (FabT f a -> FabT f b) -> Request f b
reqComposeFabT (Request r g) f  = Request r $ f . g
reqComposeFabT (ReqApp a b g) f = ReqApp a b $ f . g

-- | This only shows what has been requested, not the continuations.
instance Show (Request f a) where
  showsPrec prec = \case
    Request r _  -> showParen (prec > 10) $ showString "Request " . showsPrec 11 r
    ReqApp x y _ -> showParen (prec > 10) $ showString "ReqApp " . showsPrec 11 x . showString " " . showsPrec 11 y

instance Functor f => Functor (Request f) where
  fmap f req = reqComposeFabT req (fmap f)

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
    x <- runFabT fa
    case x of
         Res (Pure a)  -> (runFabT <$> f) a
         Res (Throw e) -> pure . Res $ Throw e
         Req req       -> pure . Req $ reqComposeFabT req (>>= f)

-- | The intermediate result 'Monad' for fabrication.
--
-- This allows us to collect requests until they are required.
data FabResult f a
   -- | 'Fab.Result.Pure' and 'Fab.Result.Thrown' values.
   = Res (Result a)
   -- | ReqApp for data.
   | Req (Request f a)
  deriving (Generic)

instance Show a => Show (FabResult f a) where
  showsPrec prec = \case
    Res r -> showsPrec prec r
    Req r -> showsPrec prec r

instance Functor f => Functor (FabResult f) where
  fmap f = \case
    Res r   -> Res $ f <$> r
    Req req -> Req $ f <$> req

instance Applicative f => Applicative (FabResult f) where
  pure = Res . Pure
  Res (Pure f)  <*> a = f <$> a
  Res (Throw e) <*> _ = Res $ Throw e
  Req req <*> Res res = Req $ reqComposeFabT req (<*> throwResult throw res)
  Req req <*> Req re' = Req $ ReqApp req re' $ throwResult throw

instance Monad f => Monad (FabResult f) where
  m >>= f = case m of
                 Res (Pure a)  -> f a
                 Res (Throw e) -> Res (Throw e)
                 Req req       -> Req $ reqComposeFabT req (>>= FabT . pure . f)

instance Monad f => Alternative (FabResult f) where
  empty = throwM Empty
  a <|> b = a `catch` (\(_ :: SomeException) -> b)

instance MonadFail f => MonadFail (FabResult f) where
  fail = throwM . Fail

instance Monad f => MonadThrow (FabResult f) where
  throwM = Res . Throw . toException

instance Monad f => MonadCatch (FabResult f) where
  catch m handle =
    case m of
         Res (Pure a)  -> pure a
         Res (Throw e) -> maybe (Res $ Throw e) handle $ fromException e
         Req req       -> Req $ reqComposeFabT req (`catch` (FabT . pure . handle))

-- | Provided for convenience for anyone who wants to throw an error in an
-- applicative context.
throw :: (HasCallStack, Exception e, Applicative f) => e -> FabT f a
throw = FabT . pure . Res . Throw . toException

instance Monad f => Alternative (FabT f) where
  empty = throw Empty
  a <|> b = a `catch` (\(_ :: SomeException) -> b)

instance MonadFail f => MonadFail (FabT f) where
  fail = throw . Fail

instance Monad f => MonadThrow (FabT f) where
  throwM = throw

instance Monad f => MonadCatch (FabT f) where
  catch fa handle = FabT $ do
    x <- runFabT fa
    case x of
         Res (Pure a)  -> pure $ pure a
         Res (Throw e) -> maybe (pure . Res $ Throw e) (runFabT <$> handle) (fromException e)
         Req req       -> pure . Req $ reqComposeFabT req (`catch` handle)

instance MonadIO f => MonadIO (FabT f) where
  liftIO f = FabT $ Res <$> liftIO (toResult f)

instance MonadTrans FabT where
  lift f = FabT $ pure <$> f

-- | Lift a computation from @f@ into @'FabT' f@, catching any exceptions.
liftCatch :: MonadCatch f => f a -> FabT f a
liftCatch f = FabT $ Res <$> toResult f
