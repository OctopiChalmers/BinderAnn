{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Peekaboo.Transformer
  ( module Peekaboo.SrcInfo
  , module Peekaboo.Wrapped
  , module Control.Monad.Annotated
  , plugin
  ) where

import GhcPlugins as GHC

import Control.Monad.Annotated
import Peekaboo.SrcInfo
import Peekaboo.Wrapped
import Peekaboo.Common

----------------------------------------
-- The entry point of this version of the plugin

plugin :: Plugin
plugin =
  defaultPlugin { parsedResultAction = const . peekaboo peekabooAnnotateMTransformer }
