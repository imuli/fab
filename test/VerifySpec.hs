{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module VerifySpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Result)

import           Control.Monad.State.Strict (StateT, modify, runStateT)
import           Data.Default (def)
import           Data.Functor.Identity (runIdentity)
import           Data.Hashable (Hashable)
import           Data.Typeable (Typeable)
import           Fab
import           GHC.Generics (Generic)

data TheData
   = Multiplicand
   | Multiplier
   | Addend
  deriving (Eq, Generic, Ord, Read, Show)

instance Hashable TheData

instance Applicative f => Fab f TheData where
  type instance Validation f TheData = Avoid
  type instance FabVal f TheData = Int
  fabricate Multiplicand = pure 1
  fabricate Multiplier   = pure 1
  fabricate Addend       = pure 0

data Verify
   = TheProduct
   | TheSum
  deriving (Eq, Generic, Ord, Read, Show)

instance Hashable Verify

instance Monad f => Fab f Verify where
  type instance Validation f Verify = VerifyTrace f Int
  type instance FabVal f Verify = Int
  fabricate TheProduct = (*) <$> fab Multiplier <*> fab Multiplicand
  fabricate TheSum     = (+) <$> fab TheProduct <*> fab Addend

spec :: Spec
spec = do
  describe "Testing Verifying Traces" $ do
    prop "Basic building." $ \a b c ->
      (Pure (a*b + c) ===) $ fst . runIdentity . flip runStateT def $ do
        modify $ putValue Multiplier (Pure a) . putValue Multiplicand (Pure b) . putValue Addend (Pure c)
        simple @FabStore $ fab TheSum
    prop "Rebuilding" $ \a b c ->
      Pure (a*b + c) === fst (runIdentity $ runStateT (rebuildRun simple a b c) def)

rebuildRun :: (Typeable f, Monad f) => Scheduler FabStore (StateT (FabStore f)) f -> Int -> Int -> Int -> StateT (FabStore f) f (Result Int)
rebuildRun sched a b c = do
  modify $ putValue Multiplier (Pure a)
  _ <- sched $ fab TheSum
  modify $ putValue Multiplicand (Pure b) . putValue Addend (Pure c)
  sched $ fab TheSum
