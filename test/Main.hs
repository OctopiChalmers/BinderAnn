module Main where

import qualified Pure
import qualified Monadic
import qualified Transformer

main :: IO ()
main = do
  putStrLn "\n== Pure ================================\n"
  Pure.tests
  putStrLn "\n== Monadic =============================\n"
  Monadic.tests
  putStrLn "\n== Monad Transformer====================\n"
  Transformer.tests
  putStrLn "\n========================================\n"
