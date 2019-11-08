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

-----------------------------------------------------------------------------
-- | An annotation monad parameterized by the type @ann@ of the annotation to
-- carry.

type Annotated ann a = AnnotatedT ann Identity a

runAnnotated :: forall ann a. Annotated ann a -> (a, StableMap ann)
runAnnotated = runIdentity . runAnnotatedT

evalAnnotated :: forall ann a. Annotated ann a -> a
evalAnnotated = fst . runAnnotated

execAnnotated :: forall ann a. Annotated ann a -> StableMap ann
execAnnotated = snd . runAnnotated

----------------------------------------
-- The mighty AnnotatedT monad transformer!

newtype AnnotatedT ann m a =
  AnnotatedT { unAnnotateT :: StateT (StableMap ann) m a }
  deriving
    ( Functor, Contravariant
    , Applicative, Alternative
    , Monad, MonadIO, MonadFail, MonadFix, MonadTrans
    )

-- runners
runAnnotatedT :: forall ann m a. AnnotatedT ann m a -> m (a, StableMap ann)
runAnnotatedT = flip runStateT emptyStableMap . unAnnotateT

evalAnnotatedT :: forall ann m a. Monad m => AnnotatedT ann m a -> m a
evalAnnotatedT m = fst <$> runAnnotatedT m

execAnnotatedT :: forall ann m a. Monad m => AnnotatedT ann m a -> m (StableMap ann)
execAnnotatedT m = snd <$> runAnnotatedT m

-- actions over annotations
createAnn_ :: Monad m => a -> ann -> AnnotatedT ann m ()
createAnn_ !a ann = AnnotatedT . StateT $ \anns ->
  return ((), insertStableNameUnsafe a ann anns)

lookupAnn_ :: Monad m => a -> AnnotatedT ann m (Maybe ann)
lookupAnn_ !a = AnnotatedT . StateT $ \anns ->
  return (lookupStableNameUnsafe a anns, anns)

instance Monad m => MonadAnnotated ann (AnnotatedT ann m) where
  createAnn = createAnn_
  lookupAnn = lookupAnn_

----------------------------------------
-- Lifting AnnotatedT to support other effects

{-# INLINE liftCallCC #-}
liftCallCC :: CallCC m (a, StableMap ann) (b, StableMap ann)
           -> CallCC (AnnotatedT ann m) a b
liftCallCC callCC_ f = AnnotatedT . StateT $ \s ->
  callCC_ $ \c ->
  runStateT (unAnnotateT (f (\a -> AnnotatedT . StateT $ \s' -> c (a, s')))) s

{-# INLINE liftCatch #-}
liftCatch :: Catch e m (a, StableMap ann) -> Catch e (AnnotatedT ann m) a
liftCatch catchE m h = AnnotatedT . StateT $ \ s ->
  runStateT (unAnnotateT m) s `catchE` \ e -> runStateT (unAnnotateT (h e)) s


{-# INLINE mapAnnotatedT #-}
mapAnnotatedT :: (m (a, StableMap ann) -> n (b, StableMap ann))
              -> AnnotatedT ann m a
              -> AnnotatedT ann n b
mapAnnotatedT f m = AnnotatedT . StateT $ \anns ->
  f (runStateT (unAnnotateT m) anns)

{-# INLINE liftListen #-}
liftListen :: Monad m
           => Listen w m (a, StableMap ann)
           -> Listen w (AnnotatedT ann m) a
liftListen listen_ m = AnnotatedT . StateT $ \anns -> do
  ~((a, anns'), w) <- listen_ (runStateT (unAnnotateT m) anns)
  return ((a, w), anns')

{-# INLINE liftPass #-}
liftPass :: Monad m
         => Pass w m (a, StableMap ann)
         -> Pass w (AnnotatedT ann m) a
liftPass pass_ m = AnnotatedT . StateT $ \anns -> pass_ $ do
    ~((a, f), anns') <- runStateT (unAnnotateT m) anns
    return ((a, anns'), f)

instance MonadCont m => MonadCont (AnnotatedT ann m) where
    callCC = liftCallCC callCC

instance MonadError e m => MonadError e (AnnotatedT ann m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

instance MonadReader r m => MonadReader r (AnnotatedT ann m) where
  ask   = lift ask
  local = mapAnnotatedT . local
  reader = lift . reader

instance MonadWriter w m => MonadWriter w (AnnotatedT ann m) where
  writer = lift . writer
  tell   = lift . tell
  listen = liftListen listen
  pass   = liftPass pass

instance MonadState s m => MonadState s (AnnotatedT ann m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadRWS r w s m => MonadRWS r w s (AnnotatedT ann m)
