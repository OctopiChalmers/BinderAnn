{-# LANGUAGE UndecidableInstances #-}
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
  -fplugin     MonadAnn.Generic
  -fplugin-opt MonadAnn.Generic:full
#-}

module Generic where

import Control.Monad.State
import Control.Monad.Except

import MonadAnn.Generic


----------------------------------------
-- Example 1: Arithmetic expressions
----------------------------------------

data EvalError =
  DivByZero (Maybe SrcInfo)
  deriving Show

type MonadEval m =
  ( MonadAnnotated SrcInfo   m
  , MonadError     EvalError m
  , MonadIO                  m
  )

type Eval = AnnotatedT SrcInfo (ExceptT EvalError IO)

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

{-# ANN test SrcInfo #-}
test :: MonadEval m => m Int
test = do
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
  runEval test >>= print
