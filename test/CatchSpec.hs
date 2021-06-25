{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module CatchSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Control.Applicative ((<|>))
import           Control.Monad.Catch (throwM)
import           Data.Functor.Identity (runIdentity)
import           Data.Hashable (Hashable)
import           Fab
import           Fab.Result (ResultException(Empty))
import           GHC.Generics (Generic)

data Catch
   = ItFails
   | ItWorks
   | ItShouldWork
  deriving (Eq, Generic, Ord, Read, Show)

instance Hashable Catch

instance (Monad f) => Fab f Catch where
  type instance Refab f Catch = Avoid
  type instance FabVal f Catch = ()
  fabricate ItFails      = throw Empty
  fabricate ItWorks      = pure ()
  fabricate ItShouldWork = fab ItFails <|> fab ItWorks

spec :: Spec
spec = do
  describe "Testing Catches" $ do
    prop "fab ItWorks" $
      Pure () === runIdentity (busy $ fab ItWorks)
    prop "fab ItFails" $
      throwM Empty === runIdentity (busy $ fab ItFails)
    prop "fab ItShouldWork" $
      Pure () === runIdentity (busy $ fab ItShouldWork)
