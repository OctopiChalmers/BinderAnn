{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Annotated.Monadic where

----------------------------------------
-- Monadic types that can be annotated using some effect

class Monad m => Annotated ann m a where
  annotateM :: ann -> m a -> m a

instance {-# OVERLAPPABLE #-} Monad m => Annotated ann m a where
  annotateM = const id
