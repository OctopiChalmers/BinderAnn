module Main where

import qualified Pure
import qualified Monadic
import qualified Generic

main :: IO ()
main = do
  putStrLn "\n== Pure ================================\n"
  Pure.tests
  putStrLn "\n== Monadic =============================\n"
  Monadic.tests
  putStrLn "\n== Generic =============================\n"
  Generic.tests
  putStrLn "\n========================================\n"
