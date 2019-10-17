{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

-- {-# OPTIONS_GHC
-- -fplugin=BinderMinder
-- -fplugin-opt=BinderMinder:|$|
-- #-}

module Main where

import Data.Maybe

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Annotated

import BinderMinder

-- the state layer
type Ref = Int

data EvalState =
  EvalState Ref [(Ref, Double)]
  deriving Show

emptyState :: EvalState
emptyState = EvalState 0 []

-- the error handling layer
data EvalError =
  VarNotFound (Maybe SrcInfo)
  | DivByZero (Maybe SrcInfo)
  deriving Show

----------------------------------------
-- the evaluation monad

type MonadEval m =
  ( MonadAnnotated SrcInfo   m
  , MonadState     EvalState m
  , MonadError     EvalError m
  , MonadIO m
  )

type Eval =
  AnnotatedT SrcInfo
  (StateT EvalState
  (ExceptT EvalError IO))

runEval :: Eval Double -> IO (Either EvalError Double)
runEval = runExceptT . flip evalStateT emptyState . evalAnnotatedT

env :: MonadEval m => Ref -> m Double
env i = do
  EvalState _ e <- get
  case lookup i e of
    Just n  -> return n
    Nothing -> lookupAnn i >>= throwError . VarNotFound

store :: MonadEval m => Double -> m Ref
store n = state $ \(EvalState u e) -> (u, EvalState (u + 1) ((u, n) : e))

binop :: MonadEval m => (Double -> Double -> Double) -> m Ref -> m Ref -> m Ref
binop f ex ey = do
  x <- env =<< ex
  y <- env =<< ey
  store (f x y)

----------------------------------------
-- the DSL

var :: MonadEval m => Ref -> m Ref
var = return

lit :: MonadEval m => Double -> m Ref
lit = store

ret :: MonadEval m => m Ref -> m Double
ret ex = env =<< ex

(.+) :: MonadEval m => m Ref -> m Ref -> m Ref
(.+) = binop (+)
(.-) :: MonadEval m => m Ref -> m Ref -> m Ref
(.-) = binop subtract
(.*) :: MonadEval m => m Ref -> m Ref -> m Ref
(.*) = binop (*)

(./) :: MonadEval m => m Ref -> m Ref -> m Ref
(./) ex ey = do
  y <- ey
  vy <- env y
  if vy /= 0
    then binop (/) ex ey
    else lookupAnn y >>= throwError . DivByZero

----------------------------------------
-- some tests

-- {-# ANN test1 SrcInfo #-}
test1 :: IO ()
test1 = evalAnnotatedT @String $ do

  xs <- return ([42] :: [Int]) `withAnn` "*x*"

  -- liftIO $ print x
  ann <- lookupAnn xs
  liftIO $ case ann of
    Nothing  -> print "boo!"
    Just str -> print $ "yay! " ++ str

  -- foo xs


foo :: [Int] -> AnnotatedT String IO ()
foo xs = do
  ann <- lookupAnn xs
  liftIO $ case ann of
    Nothing  -> print "boo!"
    Just str -> print $ "yay! " ++ str


-- test2 :: IO (Either EvalError Double)
-- test2 = runEval $ do
--   x <- var 10
--   ret (var x)



----------------------------------------
-- entry point

main :: IO ()
main = do
  putStrLn "\n----------------------------------------\n"
  test1
  putStrLn "\n----------------------------------------\n"
