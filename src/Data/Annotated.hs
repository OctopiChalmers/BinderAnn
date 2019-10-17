{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Annotated where

----------------------------------------
-- Pure types that can be annotated by default

class Annotated ann a where
  annotate :: ann -> a -> a

instance {-# OVERLAPPABLE #-} Annotated ann a where
  annotate = const id

-- Annotate the return value of a monadic computation
annotateM :: (Monad m, Annotated ann a) => ann -> m a -> m a
annotateM ann ma = annotate ann <$> ma
