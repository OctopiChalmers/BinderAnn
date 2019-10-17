{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Peekaboo.Monadic
  ( module Peekaboo.SrcInfo
  , module Peekaboo.Wrapped
  , module Data.Annotated.Monadic
  , plugin
  ) where

import GhcPlugins as GHC

import Data.Annotated.Monadic
import Peekaboo.SrcInfo
import Peekaboo.Wrapped
import Peekaboo.Common

----------------------------------------
-- The entry point of this version of the plugin

plugin :: Plugin
plugin =
  defaultPlugin { parsedResultAction = const . peekaboo peekabooAnnotateMMonadic }
