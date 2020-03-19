{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Annotated.Class where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy as LazyW
import Control.Monad.Trans.Writer.Strict as StrictW
import Control.Monad.Trans.State.Lazy as LazyS
import Control.Monad.Trans.State.Strict as StrictS
import Control.Monad.Trans.RWS.Lazy as LazyRWS
import Control.Monad.Trans.RWS.Strict as StrictRWS

import BinderAnn.SrcInfo

----------------------------------------
-- The annotation monadic effect

class Monad m => MonadAnnotated m where
  createAnn :: a -> SrcInfo -> m ()
  lookupAnn :: a -> m (Maybe SrcInfo)

-- Annotate the return value of a monadic computation
annotateM :: MonadAnnotated m => m a -> SrcInfo -> m a
annotateM ma ann = do
  a <- ma
  createAnn a ann
  return a

----------------------------------------
-- Instances for other mtl transformers

instance MonadAnnotated m => MonadAnnotated (ContT r m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated m => MonadAnnotated (ExceptT e m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated m => MonadAnnotated (IdentityT m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated m => MonadAnnotated (MaybeT m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated m => MonadAnnotated (ReaderT r m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance (Monoid w, MonadAnnotated m) => MonadAnnotated (LazyW.WriterT w m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance (Monoid w, MonadAnnotated m) => MonadAnnotated (StrictW.WriterT w m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated m => MonadAnnotated (LazyS.StateT s m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated m => MonadAnnotated (StrictS.StateT s m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance (MonadAnnotated m, Monoid w) => MonadAnnotated (LazyRWS.RWST r w s m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance (MonadAnnotated m, Monoid w) => MonadAnnotated (StrictRWS.RWST r w s m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn
