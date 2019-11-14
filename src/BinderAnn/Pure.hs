module BinderAnn.Pure
  ( module BinderAnn.SrcInfo
  , module BinderAnn.Wrapped
  , module Data.Annotated.Pure
  , plugin
  ) where

import GhcPlugins as GHC

import BinderAnn.Common
import BinderAnn.SrcInfo
import BinderAnn.Wrapped
import Data.Annotated.Pure

----------------------------------------
-- The entry point of this version of the plugin

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = const . binderann_Pure }
