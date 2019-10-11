{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Annotated where

import Prelude hiding (lookup)
import qualified Prelude

import Control.Applicative ((<|>))
import Control.Monad.Identity
import Control.Monad.State
import System.Mem.StableName
import Unsafe.Coerce
import System.IO.Unsafe

----------------------------------------
-- Dynamic stable names

-- WARNING: this is as unsafe as it gets, though it seems to work ¯\_(ツ)_/¯

data DynamicStableName = forall a. DynamicStableName (StableName a)

wrapStableName :: StableName a -> DynamicStableName
wrapStableName s = DynamicStableName (unsafeCoerce s)

makeDynamicStableName :: a -> DynamicStableName
makeDynamicStableName a = unsafePerformIO (wrapStableName <$> makeStableName a)

instance Eq DynamicStableName where
  (DynamicStableName sn) == (DynamicStableName sn') = sn `eqStableName` sn'

----------------------------------------
-- Stable maps

type StableMap a = [(DynamicStableName, a)]

instance {-# OVERLAPS #-} Show a => Show (StableMap a) where
  show m = show (snd <$> m)

empty :: StableMap v
empty = []

lookup :: a -> StableMap v -> Maybe v
lookup a = Prelude.lookup (makeDynamicStableName a)

insert :: a -> v -> StableMap v -> StableMap v
insert a v m = ins (makeDynamicStableName a) m
  where
    ins k [] = [(k, v)]
    ins k ((k', v') : xs)
      | k == k'   = (k, v)   : xs
      | otherwise = (k', v') : ins k xs

----------------------------------------
-- The mighty AnnotatedT monad transformer!

newtype AnnotatedT ann m a = AnnotatedT (StateT (StableMap ann) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (StableMap ann))

type Annotated ann a = AnnotatedT ann Identity a
type a # ann = Annotated ann a

-- this is very cute
type family (^#) m ann where
  m a ^# ann = AnnotatedT ann m a

runAnnotatedT :: forall ann m a. Monad m
              => AnnotatedT ann m a -> m (a, StableMap ann)
runAnnotatedT (AnnotatedT m) = flip runStateT empty m

evalAnnotatedT :: forall ann m a. Monad m
               => AnnotatedT ann m a -> m a
evalAnnotatedT = fmap fst <$> runAnnotatedT

execAnnotatedT :: forall ann m a. Monad m
               => AnnotatedT ann m a -> m (StableMap ann)
execAnnotatedT = fmap snd <$> runAnnotatedT


class Monad m => MonadAnnotated m where
  type Ann m :: *
  createAnn :: a -> (Ann m) -> m ()
  lookupAnn :: a -> m (Maybe (Ann m))

withAnn :: MonadAnnotated m => m a -> Ann m -> m a
withAnn io ann = do
  a <- io
  createAnn a ann
  return a

instance Monad m => MonadAnnotated (AnnotatedT ann m) where
  type Ann (AnnotatedT ann m) = ann
  createAnn !a ann = get >>= put . insert a ann
  lookupAnn !a     = get >>= return . lookup a


----------------------------------------
-- A lil test

-- I want to annotate stuff within my monad with its source location
-- And if the stuff comes from a bind, I also want to annotate it with its name


type Name = String
type Loc = (FilePath, Int, Int)
type SrcInfo = (Maybe Name, Maybe Loc)



test :: IO () ^#SrcInfo
test = do
  x1 <- return False   `withAnn` (Just "x1", Just ("this_file", 102, 2))
  y1 <- return Nothing `withAnn` (Just "y1", Just ("this_file", 103, 2))

  void (pure (Just x1 <|> y1)) `withAnn` (Nothing, Just ("this_file", 105, 2))

  -- get the annotations map
  ann <- get
  liftIO $ print ann

  -- retrieve an annotation
  ax1 <- lookupAnn x1
  liftIO $ print ax1

  -- retrieve an annotation after touching the value
  -- lookupAnn needs to be strict for this
  ax1' <- lookupAnn (if True then id x1 else False)
  liftIO $ print ax1'

  -- retrieve another anotation, just for fun
  ay1 <- lookupAnn y1
  liftIO $ print ay1


-- This seems to work as well, which is very scary
test2 :: String #SrcInfo
test2 = do
  x <- return True `withAnn` (Just "x", Just ("this_file", 150, 2))

  void (return "yay") `withAnn` (Nothing, Just ("this_file", 152, 2))

  y <- lookupAnn x >>= \case
    Just (Just nm, _) -> return nm
    _                 -> return "Nay"

  return y

main :: IO ()
main = do
  x <- runAnnotatedT test
  print x

  let Identity y = runAnnotatedT test2
  print y
