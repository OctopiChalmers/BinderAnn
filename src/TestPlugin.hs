{-#
  OPTIONS_GHC
  -fplugin     BinderMinder
  -fplugin-opt BinderMinder:|$|
#-}

module TestPlugin where

import BinderMinder

tag :: String -> IO a -> IO a
tag s io = putStrLn ("tagging " ++ show s) >> io

main :: IO ()
main = tag |$| do
  nachi <- pure (3 :: Int)
  matthi <- return 5
  foo False
  print (matthi + nachi)

{-# ANN foo "tag" #-}
foo :: Bool -> IO ()
foo True = do
  a <- pure (3 :: Int)
  b <- return 5
  print (a + b)
foo False = do
  (x, y) <- return ("hey", "ho")
  b <- return 5
  print (show b ++ " " ++ x)
