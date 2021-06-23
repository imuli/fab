{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module FactorialSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Control.Exception (ArithException(DivideByZero), toException)
import           Data.Default (def)
import           Data.Functor.Identity (runIdentity)
import           Data.Hashable (Hashable)
import           Data.Typeable (Typeable)
import           Fab

newtype Factorial n = Factorial n
  deriving (Eq, Hashable, Ord, Read, Show)

instance (Num n, Ord n, Show n, Hashable n, Typeable n, Applicative f) => Fab f (Factorial n) where
  type instance Refab f (Factorial n) = Avoid
  type instance FabVal f (Factorial n) = n
  fabricate (Factorial n) | n < 0 = throw DivideByZero
                          | n == 0 = pure 1
                          | otherwise = (n*) <$> fab (Factorial $ n - 1)

spec :: Spec
spec = do
  describe "Factorial Test for Avoid Refabber" $ do
    prop "Factorial n == factorial n, with busy" $ \(n :: Word) ->
      let nf = runIdentity $ busy $ fab $ Factorial n
       in nf === Pure (product [1..n])
    prop "Factorial -n == Throw DivideByZero" $ \(n :: Int) ->
      let nf = runIdentity $ busy $ fab $ Factorial $ negate (abs n) - 1
       in nf === Throw (toException DivideByZero)
    prop "Factorial n stores Factorial (n-1)" $ \(n :: Word) ->
      let (_, nf) = runIdentity $ simple @FabStore def $ (,) <$> fab (Factorial $ n + 1) <*> cachedFab (Factorial n)
       in nf === Pure (product [1..n+1], Just $ product [1..n])
