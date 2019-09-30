{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module BinderMinder (plugin, (@@)) where

import Data.Generics (mkM, everywhereM)

import FastString as FastString
import OccName as Name
import HsSyn as GHC
import DynFlags as GHC
import GhcPlugins as GHC

putStrLnHsc :: String -> GHC.Hsc ()
putStrLnHsc = liftIO . putStrLn

(@@) :: a -> b -> b
(@@) _ = trace "*** warning: you are using @@ but the plugin is not enabled!"

infix 2 @@

ann_tok_name :: GHC.RdrName
ann_tok_name = GHC.mkUnqual Name.varName (GHC.mkFastString "@@")

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin { GHC.parsedResultAction = binderMinder }

binderMinder :: [GHC.CommandLineOption]
             -> GHC.ModSummary
             -> GHC.HsParsedModule
             -> GHC.Hsc GHC.HsParsedModule
binderMinder _opts _summary parsed = do

  flags <- GHC.getDynFlags
  let L loc hsMod = GHC.hpm_module parsed

  hsMod' <- everywhereM (mkM (findAndAnnotate flags)) hsMod
  return parsed { GHC.hpm_module = L loc hsMod' }


findAndAnnotate :: (p ~ GHC.GhcPs)
                => GHC.DynFlags
                -> GHC.HsExpr p
                -> GHC.Hsc (GHC.HsExpr p)
findAndAnnotate flags = \case
    GHC.OpApp _
      (L l (HsVar _   (L _ ann_fun)))
      (L _ (HsVar _   (L _ ann_tok)))
      (L _ (HsDo  x y (L z doStmts)))
      | showPpr flags ann_tok ==
        showPpr flags ann_tok_name -> do

          putStrLnHsc $ "=================="
          putStrLnHsc $ "found instrumented do at: " ++ showPpr flags l
          doStmts' <- everywhereM (mkM (annotate flags ann_fun)) doStmts
          putStrLnHsc $ "=================="

          return (HsDo x y (L z doStmts'))

    expr -> return expr


annotate :: (p ~ GHC.GhcPs)
         => GHC.DynFlags
         -> GHC.RdrName
         -> GHC.ExprStmt GHC.GhcPs
         -> GHC.Hsc (GHC.ExprStmt GHC.GhcPs)
annotate flags ann_fun = \case

  -- Only annotate if the statement represents a bind operation
  stmt@(BindStmt x pat@(L _ (VarPat _ (L _ bindName))) body y z) -> do

    let bindName' = showPpr flags bindName

    let body' =
          noLoc (HsApp noExt
            (noLoc (HsApp noExt
              (noLoc (HsVar noExt
                (noLoc ann_fun)))
              (noLoc (HsLit noExt
                (HsString NoSourceText
                  (FastString.fsLit bindName'))))))
            (noLoc (HsPar noExt body)))

    let stmt' = BindStmt x pat body' y z

    putStrLnHsc $ "------------------"
    putStrLnHsc $ "*** original:\n" ++ showPpr flags stmt
    putStrLnHsc $ "*** transformed:\n" ++ showPpr flags stmt'

    return stmt'

  -- Everything that is not a bind is left unchanged
  stmt -> return stmt


    -- liftIO $ do
    --   putStrLn $ "=================="
    --   putStrLn $ "Bind stmt: " ++ showPpr flags stmt
    --   putStrLn $ "Bind name: " ++ showPpr flags bindName
    --   putStrLn $ "Bind body: " ++ showPpr flags body
    --   putStrLn $ "New body: "  ++ showPpr flags body'
    --   putStrLn $ "New stmt: "  ++ showPpr flags stmt'
    --   putStrLn $ "=================="

-- liftIO $ do
--   putStrLn $ "=================="
--   putStrLn $ "annotator: " ++ (showPpr flags ann_fun)
--   putStrLn $ "binds:"
--   mapM_ (putStrLn . showPpr flags) doBinds
--   putStrLn $ "=================="


-- forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
-- forM_ = flip mapM_

-- isBindStmt :: (p ~ GHC.GhcPs) => GHC.ExprLStmt p -> Bool
-- isBindStmt (L _ (BindStmt {})) = True
-- isBindStmt _                   = False
