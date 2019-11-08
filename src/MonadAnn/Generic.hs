module MonadAnn.Generic
  ( module MonadAnn.SrcInfo
  , module MonadAnn.Wrapped
  , module Data.Annotated.Generic
  , plugin
  ) where

import GhcPlugins as GHC

import MonadAnn.Common
import MonadAnn.SrcInfo
import MonadAnn.Wrapped
import Data.Annotated.Generic

----------------------------------------
-- The entry point of this version of the plugin

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = const . monadann __annotateM_Generic__ }
