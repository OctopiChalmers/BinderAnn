{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC
  -fplugin     BinderAnn.Generic
  -fplugin-opt BinderAnn.Generic:manual
#-}

module Generic where

import Control.Monad.State
import Control.Monad.Except

import BinderAnn.Generic

----------------------------------------
-- Example 1: Arithmetic expressions
----------------------------------------

data EvalError =
  DivByZero (Maybe SrcInfo)
  deriving Show

type MonadEval m =
  ( MonadAnnotated           m
  , MonadError     EvalError m
  , MonadIO                  m
  )

type Eval = AnnotatedT (ExceptT EvalError IO)

runEval :: Eval a -> IO (Either EvalError a)
runEval = runExceptT . evalAnnotatedT

divByZeroError :: MonadEval m => a -> m b
divByZeroError x = lookupAnn x >>= throwError . DivByZero

-- the DSL

lit :: MonadEval m => Int -> m Int
lit = return

(|+|) :: MonadEval m => Int -> Int -> m Int
(|+|) x y = lit (x + y)

(|-|) :: MonadEval m => Int -> Int -> m Int
(|-|) x y = lit (x - y)

(|*|) :: MonadEval m => Int -> Int -> m Int
(|*|) x y = lit (x * y)

(|/|) :: MonadEval m => Int -> Int -> m Int
(|/|) x y | y == 0    = divByZeroError y
          | otherwise = lit (x `div` y)


-- some tests
{-# ANN test1 SrcInfo #-}
test1 :: MonadEval m => m Int
test1 = do
  zero <- lit 0
  one  <- lit 1
  false <- return False
  liftIO $ print (zero, one)
  one |/| zero

----------------------------------------
-- entry point
----------------------------------------

tests :: IO ()
tests = do
  putStrLn "test1:"
  runEval test1 >>= print
