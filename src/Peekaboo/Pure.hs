{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Peekaboo.Pure
  ( module Peekaboo.SrcInfo
  , module Peekaboo.Wrapped
  , module Data.Annotated
  , plugin
  ) where

import GhcPlugins as GHC

import Data.Annotated
import Peekaboo.SrcInfo
import Peekaboo.Wrapped
import Peekaboo.Common

----------------------------------------
-- The entry point of this version of the plugin

plugin :: Plugin
plugin =
  defaultPlugin { parsedResultAction = const . peekaboo peekabooAnnotateMPure }
