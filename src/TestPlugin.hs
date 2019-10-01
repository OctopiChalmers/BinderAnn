{-# OPTIONS_GHC -fplugin=BinderMinder #-}

module TestPlugin where

import BinderMinder

tag :: String -> IO a -> IO a
tag s io = putStrLn ("running IO " ++ show s) >> io

main :: IO ()
main = tag @@ do
  x <- pure (3 :: Int)
  y <- return 5
  print (x + y)

main' :: IO ()
main' = do
  a <- pure (3 :: Int)
  b <- return 5
  print (a + b)
