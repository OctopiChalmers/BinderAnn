{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
module MonadAnn.Wrapped where

import MonadAnn.SrcInfo
import qualified Data.Annotated.Pure    as Pure
import qualified Data.Annotated.Monadic as Monadic
import qualified Data.Annotated.Generic as Generic

----------------------------------------
-- | Name wrappers

-- I don't want to deal with names clashes, and since the plugin can only deal
-- with raw names (RdrName), I wrap every name it uses with a weird one. Also,
-- dealing with data constructor names is a pain in the ass, so I wrap them up
-- with variable names as well. :)

-- MonadAnn.Info
__Info__ :: Maybe String -> Maybe Loc -> SrcInfo
__Info__ = Info

-- Data.Annotated.annotateM
__annotateM_Pure__ :: (Monad m, Pure.Annotated SrcInfo a) => m a -> SrcInfo -> m a
__annotateM_Pure__ = Pure.annotateM

-- Data.Annotated.Monadic.annotateM
__annotateM_Monadic__ :: Monadic.Annotated SrcInfo m a => m a -> SrcInfo -> m a
__annotateM_Monadic__ = Monadic.annotateM

  -- Control.Monad.Annotated.Class.annotateM
__annotateM_Generic__ :: Generic.MonadAnnotated SrcInfo m => m a -> SrcInfo -> m a
__annotateM_Generic__ = Generic.annotateM

-- Prelude.flip
__flip__ :: (a -> b -> c) -> (b -> a -> c)
__flip__ = flip

-- Prelude.Just
__Just__ :: a -> Maybe a
__Just__ = Just

-- Prelude.Nothing
__Nothing__ :: Maybe a
__Nothing__ = Nothing

-- | Tuple liftings
__lift_tuple_2__
  :: Monad m
  => (m a -> m a', m b -> m b')
  -> m (a, b)
  -> m (a', b')
__lift_tuple_2__ (fa, fb) m = do
  (a, b) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  return (a', b')

__lift_tuple_3__
  :: Monad m
  => (m a -> m a', m b -> m b', m c -> m c')
  -> m (a, b, c)
  -> m (a', b', c')
__lift_tuple_3__ (fa, fb, fc) m = do
  (a, b, c) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  c' <- fc (return c)
  return (a', b', c')

__lift_tuple_4__
  :: Monad m
  => (m a -> m a', m b -> m b', m c -> m c', m d -> m d')
  -> m (a, b, c, d)
  -> m (a', b', c', d')
__lift_tuple_4__ (fa, fb, fc, fd) m = do
  (a, b, c, d) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  c' <- fc (return c)
  d' <- fd (return d)
  return (a', b', c', d')

__lift_tuple_5__
  :: Monad m
  => (m a -> m a', m b -> m b', m c -> m c', m d -> m d', m e -> m e')
  -> m (a, b, c, d, e)
  -> m (a', b', c', d', e')
__lift_tuple_5__ (fa, fb, fc, fd, fe) m = do
  (a, b, c, d, e) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  c' <- fc (return c)
  d' <- fd (return d)
  e' <- fe (return e)
  return (a', b', c', d', e')
