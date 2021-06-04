{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module FactorialSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Control.Applicative (Alternative, empty)
import           Data.Default (def)
import           Data.Hashable (Hashable)
import           Data.Typeable
import           Fab

newtype Factorial n = Factorial n
  deriving (Eq, Hashable, Ord, Read, Show)

instance (Num n, Ord n, Show n, Hashable n, Typeable n, Alternative f) => Fab (Factorial n) f where
  type instance Refab (Factorial n) f = Avoid
  type instance FabVal (Factorial n) f = n
  fab f (Factorial n) | n < 0 = empty
                      | n == 0 = pure 1
                      | otherwise = (n*) <$> f (Factorial $ n - 1)

spec :: Spec
spec = do
  describe "Factorial Test for Avoid Refabber" $ do
    prop "Once n! is fabbed, (n-1)! doesn't call the scheduler." $ \(n :: Word) ->
      let facts = do
            a <- simpleFab $ Factorial (n+1)
            b <- refab (const empty) $ Factorial n
            pure (a,b)
          xy = fst <$> runFabT facts def
       in xy === Just (product [1..n+1], product [1..n])
    prop "But when n! is fabbed, (n+1)! does call the scheduler." $ \(n :: Word) ->
      let facts = do
            a <- simpleFab $ Factorial n
            b <- refab (const empty) $ Factorial (n+1)
            pure (a,b)
          xy = fst <$> runFabT facts def
       in xy === Nothing
