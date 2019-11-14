{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Annotated.Pure where

----------------------------------------
-- Pure types that can be annotated by default

class Annotated ann a where
  annotate :: a -> ann -> a

instance {-# INCOHERENT #-} Annotated ann a where
  annotate a _ = a

-- Annotate the return value of a monadic computation
annotateM :: (Monad m, Annotated ann a) => m a -> ann -> m a
annotateM ma ann = do
  a <- ma
  return (annotate a ann)
