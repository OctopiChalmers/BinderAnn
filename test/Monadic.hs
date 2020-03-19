{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC
  -fplugin     BinderAnn.Monadic
  -fplugin-opt BinderAnn.Monadic:manual
#-}

module Monadic where

import Prelude hiding (lookup)
import qualified Data.List as List

import Control.Monad.State
import Control.Monad.Except

import BinderAnn.Monadic

----------------------------------------
-- Example 1: Statefull arithmetic evaluator
----------------------------------------

-- the state layer
newtype Var = Var Int deriving (Show, Eq)
type Env = [(Var, Double)]
type CallStack = [SrcInfo]

data EvalState =
  EvalState
  { st_uniq   :: Var
  , st_env    :: Env
  , st_stack  :: CallStack
  } deriving Show

emptyState :: EvalState
emptyState = EvalState (Var 0) [] []

-- the error handling layer
data EvalError =
  VarNotFound CallStack Var
  | DivByZero CallStack
  deriving Show

----------------------------------------
-- the evaluation monad

type MonadEval m =
  ( MonadState EvalState m
  , MonadError EvalError m
  )

type Eval = StateT EvalState (ExceptT EvalError IO)

runEval :: Eval Double -> IO (Either EvalError Double)
runEval = runExceptT . flip evalStateT emptyState

getEnv :: MonadEval m => m Env
getEnv = gets st_env

getCallStack :: MonadEval m => m CallStack
getCallStack = gets st_stack

lookup :: MonadEval m => Var -> m Double
lookup i = do
  e <- getEnv
  case List.lookup i e of
    Just n  -> return n
    Nothing -> do
      stack <- getCallStack
      throwError (VarNotFound stack i)

store :: MonadEval m => Double -> m Var
store n = state $ \(EvalState (Var u) e p) ->
  (Var u, EvalState (Var (u + 1)) ((Var u, n) : e) p)

binop :: MonadEval m => (Double -> Double -> Double) -> m Var -> m Var -> m Var
binop f ex ey = do
  x <- lookup =<< ex
  y <- lookup =<< ey
  store (f x y)

----------------------------------------
-- annotation support

instance AnnotatedM Eval a where
  annotateM m info =
    -- Note; using a do statement here will produce an infinite annotation loop!
    -- In practice this shouldn't happen, as this instance won't appear in a
    -- module using the plugin.
    modify (\st -> st { st_stack = info : st_stack st }) >> m

----------------------------------------
-- the DSL

var :: MonadEval m => Var -> m Var
var = return

lit :: MonadEval m => Double -> m Var
lit = store

ret :: MonadEval m => m Var -> m Double
ret ex = lookup =<< ex

(.+) :: MonadEval m => m Var -> m Var -> m Var
(.+) = binop (+)
(.-) :: MonadEval m => m Var -> m Var -> m Var
(.-) = binop subtract
(.*) :: MonadEval m => m Var -> m Var -> m Var
(.*) = binop (*)

(./) :: MonadEval m => m Var -> m Var -> m Var
(./) ex ey = do
  y <- ey
  vy <- lookup y
  if vy /= 0
    then binop (/) ex ey
    else do
      stack <- getCallStack
      throwError (DivByZero stack)

----------------------------------------
-- tests

{-# ANN test1 SrcInfo #-}
test1 :: Eval Double
test1 = do
  x <- lit 0
  y <- lit 3 .+ var x
  ret (var y ./ var x)

{-# ANN test2 SrcInfo #-}
test2 :: Eval Double
test2 = do
  x <- lit 3 .+ var (Var 123)
  ret (var x)

-- a test for Maciej's use case
newtype Foo a = Foo a

litFoo :: Double -> Eval (Foo Var)
litFoo x = Foo <$> lit x

{-# ANN test3 SrcInfo #-}
test3 :: Eval Double
test3 = do
  Foo x <- litFoo 42
  ret (var x ./ lit 0)

----------------------------------------
-- entry point
----------------------------------------

tests :: IO ()
tests = do
  putStrLn "test1:"
  runEval test1 >>= print
  putStrLn "test2:"
  runEval test2 >>= print
  putStrLn "test3:"
  runEval test3 >>= print
  return ()
