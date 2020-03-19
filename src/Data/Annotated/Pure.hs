{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Annotated.Pure where

import BinderAnn.SrcInfo

----------------------------------------
-- Pure types that can be annotated by default

class Annotated a where
  annotate :: a -> SrcInfo -> a

instance {-# INCOHERENT #-} Annotated a where
  annotate a _ = a

-- Annotate the return value of a monadic computation
annotateM :: (Monad m, Annotated a) => m a -> SrcInfo -> m a
annotateM ma ann = do
  a <- ma
  return (annotate a ann)
