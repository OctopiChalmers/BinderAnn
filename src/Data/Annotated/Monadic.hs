{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Annotated.Monadic where

----------------------------------------
-- Monadic types that can be annotated using some effect

class Monad m => Annotated ann m a where
  annotateM :: m a -> ann -> m a

instance {-# OVERLAPPABLE #-} Monad m => Annotated ann m a where
  annotateM ma _ = ma
