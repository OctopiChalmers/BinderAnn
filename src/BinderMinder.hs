{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module BinderMinder (plugin, (@@)) where

import Data.Generics (mkM, everywhereM)
import Control.Monad

import FastString as FS
import OccName    as Name
import HsSyn      as GHC
import Module     as GHC
import DynFlags   as GHC
import GhcPlugins as GHC


putStrLnHsc :: String -> Hsc ()
putStrLnHsc = liftIO . putStrLn

(@@) :: a -> b -> b
(@@) _ = trace "*** warning: you are using @@ but the plugin is not enabled!"

infix 2 @@

ann_tok_name :: RdrName
ann_tok_name = mkUnqual Name.varName (mkFastString "@@")

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = \_opts _summary -> binderMinder }


binderMinder :: HsParsedModule -> Hsc HsParsedModule
binderMinder parsed = do

  flags <- getDynFlags
  let L loc hsMod = hpm_module parsed

  hsMod' <- removeImport flags =<< everywhereM (mkM (annotateDo flags)) hsMod
  return parsed { hpm_module = L loc hsMod' }


removeImport :: DynFlags -> HsModule GhcPs -> Hsc (HsModule GhcPs)
removeImport flags hsMod = do
  let isPlugin = \case
        (L l (ImportDecl { ideclName = L _ importName }))
          | importName == mkModuleName "BinderMinder" -> do
              putStrLnHsc $ "*** removing plugin import from: " ++ showPpr flags l
              return False
        _ -> return True

  imports <- filterM isPlugin (hsmodImports hsMod)
  return hsMod { hsmodImports = imports }


annotateDo :: DynFlags -> HsExpr GhcPs -> Hsc (HsExpr GhcPs)
annotateDo flags = \case
    OpApp _
      (L l (HsVar _   (L _ ann_fun)))
      (L _ (HsVar _   (L _ ann_tok)))
      (L _ (HsDo  x y (L z doStmts)))
      | showPpr flags ann_tok == showPpr flags ann_tok_name -> do

          putStrLnHsc $ "*** instrumenting do expression at " ++ showPpr flags l
          doStmts' <- everywhereM (mkM (annotateBind flags ann_fun)) doStmts

          return (HsDo x y (L z doStmts'))

    expr -> return expr


annotateBind :: DynFlags -> RdrName -> ExprStmt GhcPs -> Hsc (ExprStmt GhcPs)
annotateBind flags ann_fun = \case

  -- Only annotateBind if the statement represents a bind operation
  BindStmt x pat@(L _ (VarPat _ (L _ bindName))) body y z -> do

    let bindName' = showPpr flags bindName

    let body' =
          noLoc (HsApp noExt
            (noLoc (HsApp noExt
              (noLoc (HsVar noExt
                (noLoc ann_fun)))
              (noLoc (HsLit noExt
                (HsString NoSourceText
                  (fsLit bindName'))))))
            (noLoc (HsPar noExt body)))

    let stmt' = BindStmt x pat body' y z


    return stmt'

  -- Everything that is not a bind is left unchanged
  stmt -> return stmt













    -- putStrLnHsc $ "*** original:\n" ++ showPpr flags stmt
    -- putStrLnHsc $ "*** transformed:\n" ++ showPpr flags stmt'

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
