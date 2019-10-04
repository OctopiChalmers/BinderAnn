{-#
  OPTIONS_GHC
  -fplugin     BinderMinder
  -fplugin-opt BinderMinder:|$|
#-}

module TestPlugin where

import BinderMinder

tag :: Maybe BindName -> Maybe Loc -> IO a -> IO a
tag nm loc io = putStrLn ("evaluating node (" ++ show nm ++ ") at (" ++ show loc ++ ")") >> io

main :: IO Int
main = tag |$| do
  nachi <- pure 3
  matthi <- return 5
  foo False
  print (matthi + nachi)
  return nachi

{-# ANN foo "tag" #-}
foo :: Bool -> IO Int
foo True = do
  a <- pure 3
  b <- return 5
  print (a + b)
  return a
foo False = do
  (x, y) <- return ("hey", "ho")
  b <- return 5
  print (show b ++ " " ++ x)
  return b
