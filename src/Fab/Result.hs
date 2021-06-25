{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : experimental
Description : A general result type holding an exception or a value.
-}

module Fab.Result
  ( Result(..)
  , fromResult
  , throwResult
  , toResult
  , ResultException(..)
  ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Exception (Exception, SomeException(SomeException), displayException,
                     fromException, toException)
import           Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import           Data.Hashable (Hashable, hashWithSalt)
import           Data.Typeable (TypeRep, typeOf)
import           GHC.Generics (Generic)

-- | Exception Type for implementing 'Alternative' and 'MonadFail'.
data ResultException
   = Empty
   | Fail String
  deriving (Eq, Generic, Ord, Read)

instance Show ResultException where
  show = displayException

instance Exception ResultException where
  displayException Empty      = "empty"
  displayException (Fail msg) = "failure due to " <> msg

-- | Result type, isomorphic to @'Either' 'SomeException' a@, but specialized
-- to be able to provide 'Eq', 'Ord', and 'Hashable' instances.
data Result a
   = Throw SomeException
   | Pure a
  deriving (Foldable, Functor, Generic, Show, Traversable)

-- | Throw any exceptions in a 'Result' in to a @f@, using supplied throwing
-- method, which allows throwing into applicatives.
throwResult :: Applicative f => (SomeException -> f a) -> Result a -> f a
throwResult throw (Throw e) = throw e
throwResult _     (Pure e)  = pure e

-- | Throw any exceptions in a 'Result' in to a @f@.
--
-- Ideally this would work with 'Applicative's also.
fromResult :: MonadThrow f => Result a -> f a
fromResult = throwResult throwM

-- | Catch any exceptions resulting from a computation.
--
-- Note, this currently catches *all* exceptions, including asynchronous ones
-- that it probably shouldn't.
toResult :: MonadCatch f => f a -> f (Result a)
toResult fa = catch (Pure <$> fa) (pure . Throw)

instance Applicative Result where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Throw e <*> _     = Throw e
  _ <*> Throw e     = Throw e

instance Alternative Result where
  empty = Throw $ toException Empty
  Pure a <|> _  = Pure a
  _ <|> Pure b  = Pure b
  _ <|> Throw b = Throw b

instance Monad Result where
  Pure a >>= f  = f a
  Throw e >>= _ = Throw e

instance MonadFail Result where
  fail = Throw . toException . Fail

instance MonadThrow Result where
  throwM = Throw . toException

instance MonadCatch Result where
  catch (Pure a) _        = Pure a
  catch (Throw e) handler = maybe (Throw e) handler (fromException e)

exceptType :: SomeException -> TypeRep
exceptType (SomeException e) = typeOf e

instance Eq a => Eq (Result a) where
  Throw e == Throw e' = exceptType e == exceptType e' && show e == show e'
  Pure x == Pure y    = x == y
  _ == _              = False

instance Ord a => Ord (Result a) where
  compare (Throw e) (Throw e') = compare (exceptType e, show e) (exceptType e', show e')
  compare (Throw _) _          = LT
  compare (Pure x)   (Pure y)  = compare x y
  compare (Pure _) _           = GT

instance Hashable a => Hashable (Result a) where
  hashWithSalt salt (Throw e) = hashWithSalt salt (exceptType e, show e)
  hashWithSalt salt (Pure x)  = hashWithSalt salt x
