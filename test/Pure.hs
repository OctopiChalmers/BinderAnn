{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fplugin=Peekaboo.Pure #-}

module Pure where

import Control.Monad.Writer
import Control.Monad.Identity

import Peekaboo.Pure

data Exp =
    Val Int
  | Add Exp Exp
  | Ann SrcInfo Exp
  deriving Show

type Eval = WriterT [SrcInfo] Identity

instance Annotated SrcInfo Exp where
  annotate = Ann

runEval :: Eval Exp -> (Int, [SrcInfo])
runEval e = runIdentity (runWriterT (eval =<< e))

runEval' :: Eval Exp -> Exp
runEval' = fst . runIdentity . runWriterT

eval :: Exp -> Eval Int
eval (Val n) = return n
eval (Add x y) = liftM2 (+) (eval x) (eval y)
eval (Ann a x) = tell [a] >> eval x

val :: Int -> Eval Exp
val n = return (Val n)

(|+|) :: Exp -> Exp -> Eval Exp
x |+| y = return (Add x y)

{-# ANN test1 SrcInfo #-}
test1 :: Eval Exp
test1 = do
  x  <- val 5
  y  <- val 10
  z  <- return False
  w  <- (if z then val 1 else val 2)
  s1 <- x |+| y
  s1 |+| w

tests :: IO ()
tests = do
  putStrLn "test1:"
  print (runEval  test1)
  print (runEval' test1)
