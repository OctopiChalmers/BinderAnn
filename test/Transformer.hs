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

{-# OPTIONS_GHC -fplugin=Peekaboo.Transformer #-}

module Transformer where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Annotated

import Peekaboo.Transformer


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
-- Example 2: Propositional logic
----------------------------------------

-- type Id = Int

-- data Expr =
--     Var Id
--   | Neg Expr
--   | And Expr Expr
--   | Or  Expr Expr
--   | Imp Expr Expr
--   deriving Show

-- data PropError =
--     Inconsistent
--   | UnboundVariable
--   deriving Show

-- data PropState = PropState { uniq :: Int, props :: [(Id, Expr)] }

-- emptyPropState :: PropState
-- emptyPropState = PropState 0 []

-- type MonadProp m =
--   ( MonadAnnotated SrcInfo m
--   , MonadError PropError m
--   , MonadState PropState m
--   )

-- type Prop = AnnotatedT SrcInfo (State PropState)



-- -- the dsl

-- class MonadProp m => Vars m t where
--   vars :: m t

-- instance {-# OVERLAPPABLE #-} MonadProp m => Vars m Id where
--   vars = new

-- instance MonadProp m => Vars m (Id, Id) where
--   vars = liftM2 (,) new new

-- instance MonadProp m => Vars m (Id, Id, Id) where
--   vars = liftM3 (,,) new new new

-- instance MonadProp m => Vars m (Id, Id, Id, Id) where
--   vars = liftM4 (,,,) new new new new


-- new :: MonadProp m => m Id
-- new = state $ \s -> (uniq s, s { uniq = uniq s + 1})

-- var :: MonadProp m => Id -> m Id
-- var = return

-- neg :: MonadProp m => m Id -> m Id
-- neg e = undefined

-- (/\) :: MonadProp m => m Id -> m Id -> m Id
-- (/\) x y = undefined

-- (\/) :: MonadProp m => m Id -> m Id -> m Id
-- (\/) x y = undefined

-- (==>) :: MonadProp m => m Id -> m Id -> m Id
-- (==>) x y = undefined

-- tautology :: MonadProp m => m Id -> m Bool
-- tautology = undefined


-- -- some tests

-- prop_test :: MonadProp m => m Bool
-- prop_test = do
--   (p, q, r) <- vars

--   prop1 <- var p /\ var q
--   prop2 <- var p \/ neg (var prop1)
--   prop3 <- var r ==> (var q \/ var prop2)

--   tautology ((var prop1 /\ var prop2) ==> var prop3)


----------------------------------------
-- entry point
----------------------------------------

tests :: IO ()
tests = do
  putStrLn "test1:"
  runEval test >>= print
