module MonadAnn.Monadic
  ( module MonadAnn.SrcInfo
  , module MonadAnn.Wrapped
  , module Data.Annotated.Monadic
  , plugin
  ) where

import GhcPlugins as GHC

import MonadAnn.Common
import MonadAnn.SrcInfo
import MonadAnn.Wrapped
import Data.Annotated.Monadic

----------------------------------------
-- The entry point of this version of the plugin

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = const . monadann_Monadic }
