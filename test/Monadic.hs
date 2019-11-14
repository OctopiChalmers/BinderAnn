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

{-# OPTIONS_GHC -fplugin=BinderAnn.Monadic #-}

module Monadic where

import Control.Monad.State
import Control.Monad.Except

import BinderAnn.Monadic


----------------------------------------
-- Example 1: Arithmetic expressions
----------------------------------------


----------------------------------------
-- entry point
----------------------------------------

tests :: IO ()
tests = do
  -- putStrLn "test1:"
  -- runEval test >>= print
  return ()
