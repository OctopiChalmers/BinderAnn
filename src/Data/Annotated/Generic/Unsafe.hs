{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}
module Data.Annotated.Generic.Unsafe
  ( Annotated
  , runAnnotated, evalAnnotated, execAnnotated
  , AnnotatedT(..)
  , runAnnotatedT, evalAnnotatedT, execAnnotatedT
  , module Control.Monad
  , module Control.Monad.Fix
  , module Control.Monad.Annotated.Class
  ) where

import Data.Functor.Contravariant
import Control.Applicative
import Control.Monad
import Control.Monad.Signatures
import Control.Monad.Fix
import Control.Monad.State
import Control.Monad.Fail
import Control.Monad.Identity (Identity, runIdentity)

import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.RWS.Class

import System.Mem.StableMap
import Control.Monad.Annotated.Class

import BinderAnn.SrcInfo

-----------------------------------------------------------------------------
-- | An annotation monad parameterized by the type @ann@ of the annotation to
-- carry.

type Annotated a = AnnotatedT Identity a

runAnnotated :: forall a. Annotated a -> (a, StableMap SrcInfo)
runAnnotated = runIdentity . runAnnotatedT

evalAnnotated :: forall a. Annotated a -> a
evalAnnotated = fst . runAnnotated

execAnnotated :: forall a. Annotated a -> StableMap SrcInfo
execAnnotated = snd . runAnnotated

----------------------------------------
-- The mighty AnnotatedT monad transformer!

newtype AnnotatedT m a =
  AnnotatedT { unAnnotateT :: StateT (StableMap SrcInfo) m a }
  deriving
    ( Functor, Contravariant
    , Applicative, Alternative
    , Monad, MonadIO, MonadFail, MonadFix, MonadTrans
    )

-- runners
runAnnotatedT :: forall m a. AnnotatedT m a -> m (a, StableMap SrcInfo)
runAnnotatedT = flip runStateT emptyStableMap . unAnnotateT

evalAnnotatedT :: forall m a. Monad m => AnnotatedT m a -> m a
evalAnnotatedT m = fst <$> runAnnotatedT m

execAnnotatedT :: forall m a. Monad m => AnnotatedT m a -> m (StableMap SrcInfo)
execAnnotatedT m = snd <$> runAnnotatedT m

-- actions over annotations
createAnn_ :: Monad m => a -> SrcInfo -> AnnotatedT m ()
createAnn_ !a ann = AnnotatedT . StateT $ \anns ->
  return ((), insertStableNameUnsafe a ann anns)

lookupAnn_ :: Monad m => a -> AnnotatedT m (Maybe SrcInfo)
lookupAnn_ !a = AnnotatedT . StateT $ \anns ->
  return (lookupStableNameUnsafe a anns, anns)

instance Monad m => MonadAnnotated (AnnotatedT m) where
  createAnn = createAnn_
  lookupAnn = lookupAnn_

----------------------------------------
-- Lifting AnnotatedT to support other effects

{-# INLINE liftCallCC #-}
liftCallCC :: CallCC m (a, StableMap SrcInfo) (b, StableMap SrcInfo)
           -> CallCC (AnnotatedT m) a b
liftCallCC callCC_ f = AnnotatedT . StateT $ \s ->
  callCC_ $ \c ->
  runStateT (unAnnotateT (f (\a -> AnnotatedT . StateT $ \s' -> c (a, s')))) s

{-# INLINE liftCatch #-}
liftCatch :: Catch e m (a, StableMap SrcInfo) -> Catch e (AnnotatedT m) a
liftCatch catchE m h = AnnotatedT . StateT $ \ s ->
  runStateT (unAnnotateT m) s `catchE` \ e -> runStateT (unAnnotateT (h e)) s


{-# INLINE mapAnnotatedT #-}
mapAnnotatedT :: (m (a, StableMap SrcInfo) -> n (b, StableMap SrcInfo))
              -> AnnotatedT m a
              -> AnnotatedT n b
mapAnnotatedT f m = AnnotatedT . StateT $ \anns ->
  f (runStateT (unAnnotateT m) anns)

{-# INLINE liftListen #-}
liftListen :: Monad m
           => Listen w m (a, StableMap SrcInfo)
           -> Listen w (AnnotatedT m) a
liftListen listen_ m = AnnotatedT . StateT $ \anns -> do
  ~((a, anns'), w) <- listen_ (runStateT (unAnnotateT m) anns)
  return ((a, w), anns')

{-# INLINE liftPass #-}
liftPass :: Monad m
         => Pass w m (a, StableMap SrcInfo)
         -> Pass w (AnnotatedT m) a
liftPass pass_ m = AnnotatedT . StateT $ \anns -> pass_ $ do
    ~((a, f), anns') <- runStateT (unAnnotateT m) anns
    return ((a, anns'), f)

instance MonadCont m => MonadCont (AnnotatedT m) where
    callCC = liftCallCC callCC

instance MonadError e m => MonadError e (AnnotatedT m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

instance MonadReader r m => MonadReader r (AnnotatedT m) where
  ask   = lift ask
  local = mapAnnotatedT . local
  reader = lift . reader

instance MonadWriter w m => MonadWriter w (AnnotatedT m) where
  writer = lift . writer
  tell   = lift . tell
  listen = liftListen listen
  pass   = liftPass pass

instance MonadState s m => MonadState s (AnnotatedT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadRWS r w s m => MonadRWS r w s (AnnotatedT m)
