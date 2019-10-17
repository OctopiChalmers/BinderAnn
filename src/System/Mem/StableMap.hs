{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MagicHash #-}
module System.Mem.StableMap where

import Control.Monad.IO.Class
import System.Mem.StableName
import System.IO.Unsafe
import Unsafe.Coerce
import GHC.Types (Any)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- import GHC.Base (stableNameToInt#)

----------------------------------------
-- Dynamic stable names

newtype DynStableName = DynStableName (StableName Any)

instance Eq DynStableName where
  DynStableName sn == DynStableName sn' = sn `eqStableName` sn'

makeDynStableName :: MonadIO m => a -> m DynStableName
makeDynStableName a =
  liftIO (wrapStableName <$> makeStableName a)

makeDynStableNameUnsafe :: a -> DynStableName
makeDynStableNameUnsafe a =
  unsafePerformIO (wrapStableName <$> makeStableName a)

wrapStableName :: StableName a -> DynStableName
wrapStableName s = DynStableName (unsafeCoerce s)

hashDynStableName :: DynStableName -> Int
hashDynStableName (DynStableName sn) = hashStableName sn

----------------------------------------
-- Stable maps

type StableMap a = IntMap [(DynStableName, a)]

emptyStableMap :: StableMap v
emptyStableMap = IntMap.empty

insertStableName :: MonadIO m => a -> b -> StableMap b -> m (StableMap b)
insertStableName a b sm = do
  sna <- makeDynStableName a
  return (IntMap.insertWith (<>) (hashDynStableName sna) [(sna,b)] sm)

lookupStableName :: MonadIO m => a -> StableMap b -> m (Maybe b)
lookupStableName a sm = do
  sna <- makeDynStableName a
  case IntMap.lookup (hashDynStableName sna) sm of
    Just pairs -> return (Prelude.lookup sna pairs)
    Nothing    -> return Nothing

insertStableNameUnsafe :: a -> b -> StableMap b -> StableMap b
insertStableNameUnsafe a b sm =
  let sna = makeDynStableNameUnsafe a
  in IntMap.insertWith insertReplace' (hashDynStableName sna) [(sna,b)] sm


lookupStableNameUnsafe :: a -> StableMap b -> Maybe b
lookupStableNameUnsafe a sm =
  let sna = makeDynStableNameUnsafe a
  in case IntMap.lookup (hashDynStableName sna) sm of
       Just pairs -> Prelude.lookup sna pairs
       Nothing    -> Nothing

insertReplace' :: [(DynStableName, a)] -> [(DynStableName, a)] -> [(DynStableName, a)]
insertReplace' ((k,v):_) = insertReplace k v
insertReplace' _         = id

insertReplace :: DynStableName -> a -> [(DynStableName, a)] -> [(DynStableName, a)]
insertReplace k v [] = [(k, v)]
insertReplace k v ((k', v') : xs)
 | k == k'   = (k, v)   : xs
 | otherwise = (k', v') : insertReplace k v xs
