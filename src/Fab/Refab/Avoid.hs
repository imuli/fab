{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Copyright   : Unlicense (Public Domain)
Stability   : stable
Description : The Avoiding Refabber.
-}

module Fab.Refab.Avoid
  ( Avoid
  ) where

import           Data.Default (Default, def)
import           Fab.Core (Fab, Refabber, verify)

-- | Only fabricate a value when none is present.
--
-- Only sensible for values with no dependencies (which may still be overridden
-- by adding entries to the store), values that are fully determined by the
-- key, where fab only provides memoization, or you want to use the same values
-- for the duration of your session.
--
-- This is the strategy employed by Haxl.
data Avoid
   = Avoid
  deriving (Show)

instance Default Avoid where
  def = Avoid

instance Fab k f => Refabber Avoid k f where
  verify _ _ _ _ = pure True
