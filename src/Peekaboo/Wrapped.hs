{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Peekaboo.Wrapped where

import qualified Data.Annotated as Pure
import qualified Data.Annotated.Monadic as Monadic
import qualified Control.Monad.Annotated.Class as Transformer

import Peekaboo.SrcInfo

----------------------------------------
-- | Name wrappers

-- I don't want to deal with names clashes, and since the plugin can only deal
-- with raw names (RdrName), I wrap every name it uses with a weird one. Also,
-- dealing with data constructor names is a pain in the ass, so I wrap them up
-- with variable names as well. :)

-- Peekaboo.SrcInfo
__Peekaboo_SrcInfo__ :: Maybe String -> Maybe Loc -> SrcInfo
__Peekaboo_SrcInfo__ = MkSrcInfo

-- Data.Annotated.annotateM
__Peekaboo_Data_Annotated_annotateM__
  :: (Monad m, Pure.Annotated ann a)
  => ann -> m a -> m a
__Peekaboo_Data_Annotated_annotateM__ =
  Pure.annotateM

-- Data.Annotated.Monadic.annotateM
__Peekaboo_Data_Annotated_Monadic_annotateM__
  :: Transformer.MonadAnnotated SrcInfo m
  => SrcInfo -> m a -> m a
__Peekaboo_Data_Annotated_Monadic_annotateM__ =
  Monadic.annotateM

  -- Control.Monad.Annotated.Class.annotateM
__Peekaboo_Control_Monad_Annotated_annotateM__
  :: Transformer.MonadAnnotated SrcInfo m
  => SrcInfo -> m a -> m a
__Peekaboo_Control_Monad_Annotated_annotateM__ =
  Transformer.annotateM

-- Prelude.flip
__Peekaboo_flip__ :: (a -> b -> c) -> (b -> a -> c)
__Peekaboo_flip__ = flip

-- Prelude.Just
__Peekaboo_Just__ :: a -> Maybe a
__Peekaboo_Just__ = Just

-- Prelude.Nothing
__Peekaboo_Nothing__ :: Maybe a
__Peekaboo_Nothing__ = Nothing

-- | Tuple liftings
__Peekaboo_lift_tuple_2__
  :: Monad m
  => (m a -> m a', m b -> m b')
  -> m (a, b)
  -> m (a', b')
__Peekaboo_lift_tuple_2__ (fa, fb) m = do
  (a, b) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  return (a', b')

__Peekaboo_lift_tuple_3__
  :: Monad m
  => (m a -> m a', m b -> m b', m c -> m c')
  -> m (a, b, c)
  -> m (a', b', c')
__Peekaboo_lift_tuple_3__ (fa, fb, fc) m = do
  (a, b, c) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  c' <- fc (return c)
  return (a', b', c')

__Peekaboo_lift_tuple_4__
  :: Monad m
  => (m a -> m a', m b -> m b', m c -> m c', m d -> m d')
  -> m (a, b, c, d)
  -> m (a', b', c', d')
__Peekaboo_lift_tuple_4__ (fa, fb, fc, fd) m = do
  (a, b, c, d) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  c' <- fc (return c)
  d' <- fd (return d)
  return (a', b', c', d')

__Peekaboo_lift_tuple_5__
  :: Monad m
  => (m a -> m a', m b -> m b', m c -> m c', m d -> m d', m e -> m e')
  -> m (a, b, c, d, e)
  -> m (a', b', c', d', e')
__Peekaboo_lift_tuple_5__ (fa, fb, fc, fd, fe) m = do
  (a, b, c, d, e) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  c' <- fc (return c)
  d' <- fd (return d)
  e' <- fe (return e)
  return (a', b', c', d', e')
