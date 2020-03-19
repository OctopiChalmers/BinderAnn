{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Annotated.Monadic where

import BinderAnn.SrcInfo

----------------------------------------
-- Monadic types that can be annotated using some effect

class Monad m => AnnotatedM m a where
  annotateM :: m a -> SrcInfo -> m a

instance {-# INCOHERENT #-} Monad m => AnnotatedM m a where
  annotateM ma _ = ma
