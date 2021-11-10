module BinderAnn.Monadic
  ( module BinderAnn.SrcInfo
  , module BinderAnn.Wrapped
  , module Data.Annotated.Monadic
  , plugin
  ) where

import GHC.Plugins as GHC

import BinderAnn.Common
import BinderAnn.SrcInfo
import BinderAnn.Wrapped
import Data.Annotated.Monadic

----------------------------------------
-- The entry point of this version of the plugin

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = const . binderann_Monadic }
