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

----------------------------------------
-- The annotation monadic effect

class Monad m => MonadAnnotated ann m where
  createAnn :: a -> ann -> m ()
  lookupAnn :: a -> m (Maybe ann)

-- Annotate the return value of a monadic computation
annotateM :: MonadAnnotated ann m => m a -> ann -> m a
annotateM ma ann = do
  a <- ma
  createAnn a ann
  return a

instance {-# OVERLAPPABLE #-} Monad m => MonadAnnotated ann m where
  createAnn _ _ = return ()
  lookupAnn _   = return Nothing

----------------------------------------
-- Instances for other mtl transformers

instance MonadAnnotated ann m => MonadAnnotated ann (ContT r m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated ann m => MonadAnnotated ann (ExceptT e m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated ann m => MonadAnnotated ann (IdentityT m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated ann m => MonadAnnotated ann (MaybeT m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated ann m => MonadAnnotated ann (ReaderT r m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance (Monoid w, MonadAnnotated ann m) => MonadAnnotated ann (LazyW.WriterT w m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance (Monoid w, MonadAnnotated ann m) => MonadAnnotated ann (StrictW.WriterT w m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated ann m => MonadAnnotated ann (LazyS.StateT s m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance MonadAnnotated ann m => MonadAnnotated ann (StrictS.StateT s m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance (MonadAnnotated ann m, Monoid w) => MonadAnnotated ann (LazyRWS.RWST r w s m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn

instance (MonadAnnotated ann m, Monoid w) => MonadAnnotated ann (StrictRWS.RWST r w s m) where
  createAnn a = lift . createAnn a
  lookupAnn = lift . lookupAnn
