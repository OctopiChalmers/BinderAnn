{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module BinderMinder
  ( BindName, Loc
  , plugin
  , (@@)
  , __BinderMinder_Just__
  , __BinderMinder_Nothing__
  , __BinderMinder_lift_tuple_2__
  , __BinderMinder_lift_tuple_3__
  , __BinderMinder_lift_tuple_4__
  , __BinderMinder_lift_tuple_5__
  ) where

import Data.Generics (mkM, everywhereM, listify)
import Control.Monad

import FastString as FS
import OccName    as Name
import HsSyn      as GHC
import DynFlags   as GHC
import GhcPlugins as GHC

type BindName = String
type Loc      = (FilePath, Int, Int) -- file, line and column

type Anns = [(RdrName, RdrName)]

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = \opts _summ -> binderMinder opts }

binderMinder :: [CommandLineOption] -> HsParsedModule -> Hsc HsParsedModule
binderMinder opts parsed = do
  putStrLnHsc $ "[!] starting BinderMinder plugin"

  flags <- getDynFlags
  let L loc hsMod = hpm_module parsed

  -- create an annotation token based on the plugin cli options (@@ by default)
  let ann_tok = case opts of [sym] -> mkRdrName sym; _ -> mkRdrName "@@"

  -- extract all the annotation pragmas from the code
  let anns = extractAnn <$> listify isAnn hsMod

  let transform = everywhereM (mkM (annotateTokenAnnDo flags ann_tok))
             >=> everywhereM (mkM (annotateTopLevelAnnDo flags anns))

  hsMod' <- transform hsMod
  return parsed { hpm_module = L loc hsMod' }


-- | Search for top-level bindings of the shape:
-- foo = do ...
-- where foo has an ANN annotation
annotateTopLevelAnnDo :: DynFlags -> Anns -> Match GhcPs (LHsExpr GhcPs)
               -> Hsc (Match GhcPs (LHsExpr GhcPs))
annotateTopLevelAnnDo flags anns = \case
  -- annotate match statements that appear in the module annotations
  Match m_x m_ctx m_ps
    (GRHSs grhss_x
      [L l (GRHS grhs_x grhs_guards
             (L l' (HsDo do_x do_cxt (L l'' doStmts))))] lbs)
    | Just ann_fun <- lookup (unLoc (mc_fun m_ctx)) anns -> do
        putStrLnHsc $ "[!] annotating do expression at " ++ showPpr flags l

        doStmts' <- mapM (annotateBind flags ann_fun) doStmts
        return (Match m_x m_ctx m_ps
                 (GRHSs grhss_x
                   [L l (GRHS grhs_x grhs_guards
                          (L l' (HsDo do_x do_cxt (L l'' doStmts'))))] lbs))

  -- otherwise, just return the match unchanged
  match -> return match

-- | Search for annotated do expressions of the shape:
-- `foo = ann_fun <ann_tok> do ...`
annotateTokenAnnDo :: DynFlags -> RdrName -> HsExpr GhcPs -> Hsc (HsExpr GhcPs)
annotateTokenAnnDo flags ann_tok = \case
  -- annotate do expression prefixed with the annotation token
  OpApp _
    (L l (HsVar _   (L _ ann_fun)))
    (L _ (HsVar _   (L _ tok)))
    (L _ (HsDo  x y (L z doStmts)))
    | showPpr flags tok == showPpr flags ann_tok -> do
        putStrLnHsc $ "[!] annotating do expression at " ++ showPpr flags l

        doStmts' <- mapM (annotateBind flags ann_fun) doStmts
        return (HsDo x y (L z doStmts'))

  -- otherwise, just return the expression unchanged
  expr -> return expr


annotateBind :: DynFlags -> RdrName -> ExprLStmt GhcPs -> Hsc (ExprLStmt GhcPs)
annotateBind flags ann_fun = \case

  ----------------------------------------
  -- Bind statements where the lhs is a variable pattern
  (L l (BindStmt x pat@(L _ (VarPat _ (L _ bindName))) body y z)) -> do
    putStrLnHsc $ "[+] annotating single bind \""++ showPpr flags bindName
               ++ "\" at " ++ showPpr flags l

    let bindStr = varPatToStringLit flags pat
    let body' = noLoc $ HsApp noExt
                (noLoc $ HsApp noExt
                  (noLoc $ HsApp noExt
                    (noLoc $ HsVar noExt (noLoc ann_fun))
                    (noLoc $ HsPar noExt
                      (noLoc $ HsApp noExt
                        (noLoc $ HsVar noExt $ noLoc $
                          mkRdrName "__BinderMinder_Just__")
                        (noLoc $ HsLit noExt $ bindStr))))
                  (noLoc $ mkSrcSpanExpr l))
                (noLoc $ HsPar noExt body)

    return (L l (BindStmt x pat body' y z))

  ----------------------------------------
  -- Bind statements where the lhs is a tuple pattern
  (L l (BindStmt x pat@(L _ (TuplePat _ binds _)) body y z))
    | length binds > 1 &&
      length binds <= 5 &&
      all isVarPat binds -> do
    putStrLnHsc $ "[+] annotating tuple bind \""++ showPpr flags pat
               ++ "\" at " ++ showPpr flags l

    let bindStrs = varPatToStringLit flags <$> binds
    let annTup = mkAnnTuple ann_fun l bindStrs
    let lifter = mkRdrName ("__BinderMinder_lift_tuple_"++ show (length binds) ++"__")

    -- let bindStr = HsString NoSourceText (fsLit (showPpr flags bindName))
    let body' = noLoc $ HsApp noExt
                (noLoc $ HsApp noExt
                  (noLoc $ HsVar noExt (noLoc lifter))
                  (noLoc annTup))
                (noLoc $ HsPar noExt body)

    return (L l (BindStmt x pat body' y z))

  ----------------------------------------
  -- Body statements
  (L l (BodyStmt x body y z)) -> do
    putStrLnHsc $ "[+] annotating body statement at " ++ showPpr flags l

    let body' = noLoc $ HsApp noExt
                (noLoc $ HsApp noExt
                  (noLoc $ HsApp noExt
                    (noLoc $ HsVar noExt $ noLoc ann_fun)
                    (noLoc $ HsVar noExt $ noLoc $ mkRdrName "__BinderMinder_Nothing__"))
                  (noLoc $ mkSrcSpanExpr l))
                (noLoc $ HsPar noExt body)

    return (L l (BodyStmt x body' y z))

  -- Everything that is not a bind is left unchanged
  stmt -> return stmt




----------------------------------------
-- | Helper functions

putStrLnHsc :: String -> Hsc ()
putStrLnHsc = liftIO . putStrLn

mkRdrName :: String -> RdrName
mkRdrName = mkUnqual Name.varName . mkFastString

-- | Check whether an annotation pragma is of the shape:
-- | {-# ANN ident "str" #-}
isAnn :: AnnDecl GhcPs -> Bool
isAnn = \case
  (HsAnnotation _ _
    (ValueAnnProvenance (L _ _))
    (L _ (HsLit _ (HsString _ _)))) -> True
  _                                 -> False

extractAnn :: AnnDecl GhcPs -> (RdrName, RdrName)
extractAnn = \case
  (HsAnnotation _ _
    (ValueAnnProvenance (L _ target_fun))
    (L _ (HsLit _ (HsString _ ann_fun)))) -> (target_fun, mkVarUnqual ann_fun)
  _                                       -> error "this should not happen"

isVarPat :: LPat GhcPs -> Bool
isVarPat = \case
  (L _ (VarPat {})) -> True
  _                 -> False

varPatToStringLit :: DynFlags -> LPat GhcPs -> HsLit GhcPs
varPatToStringLit flags = \case
  (L _ (VarPat _ (L _ bindName))) ->
    HsString NoSourceText (fsLit (showPpr flags bindName))
  _ -> error "this should not happen"

mkAnnTuple :: RdrName -> SrcSpan -> [HsLit GhcPs] -> HsExpr GhcPs
mkAnnTuple ann_fun loc bindStrs =
  ExplicitTuple noExt (mkLHsTupArg <$> bindStrs) Boxed
  where
    mkLHsTupArg bindStr =
      noLoc $ Present noExt $
        noLoc $ HsApp noExt
          (noLoc $ HsApp noExt
            (noLoc $ HsVar noExt $ noLoc ann_fun)
            (noLoc $ HsPar noExt
              (noLoc $ HsApp noExt
                (noLoc $ HsVar noExt $ noLoc $
                  mkRdrName "__BinderMinder_Just__")
                (noLoc $ HsLit noExt $ bindStr))))
            (noLoc $ mkSrcSpanExpr loc)

mkNumLit :: Int -> HsExpr GhcPs
mkNumLit n = HsLit noExt (HsInt noExt (mkIntegralLit n))

mkStrLit :: FastString -> HsExpr GhcPs
mkStrLit s = HsLit noExt (HsString NoSourceText s)

mkSrcSpanExpr :: SrcSpan -> HsExpr GhcPs
mkSrcSpanExpr (RealSrcSpan loc) =
  HsPar noExt $ noLoc $
    HsApp noExt
      (noLoc $ HsVar noExt $ noLoc $
        mkRdrName "__BinderMinder_Just__")
      (noLoc $ ExplicitTuple noExt tuple Boxed)
  where tuple = [ noLoc $ Present noExt $ noLoc (mkStrLit (srcSpanFile loc))
                , noLoc $ Present noExt $ noLoc (mkNumLit (srcSpanStartLine loc))
                , noLoc $ Present noExt $ noLoc (mkNumLit (srcSpanStartCol loc))
                ]
mkSrcSpanExpr (UnhelpfulSpan {}) =
  HsVar noExt $ noLoc $ mkRdrName "__BinderMinder_Nothing__"


----------------------------------------
-- | Make the default annotation token an actual function.
-- This way we can at least raise a warning or something

(@@) :: a -> b -> b
(@@) _ = trace "*** warning: you are using @@ but the plugin is not enabled!"
infix 2 @@

----------------------------------------
-- | Lift tuple returning operations

__BinderMinder_Nothing__ :: Maybe a
__BinderMinder_Nothing__ = Nothing

__BinderMinder_Just__ :: a -> Maybe a
__BinderMinder_Just__ = Just

__BinderMinder_lift_tuple_2__
  :: Monad m
  => (m a -> m a', m b -> m b')
  -> m (a, b)
  -> m (a', b')
__BinderMinder_lift_tuple_2__ (fa, fb) m = do
  (a, b) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  return (a', b')

__BinderMinder_lift_tuple_3__
  :: Monad m
  => (m a -> m a', m b -> m b', m c -> m c')
  -> m (a, b, c)
  -> m (a', b', c')
__BinderMinder_lift_tuple_3__ (fa, fb, fc) m = do
  (a, b, c) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  c' <- fc (return c)
  return (a', b', c')

__BinderMinder_lift_tuple_4__
  :: Monad m
  => (m a -> m a', m b -> m b', m c -> m c', m d -> m d')
  -> m (a, b, c, d)
  -> m (a', b', c', d')
__BinderMinder_lift_tuple_4__ (fa, fb, fc, fd) m = do
  (a, b, c, d) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  c' <- fc (return c)
  d' <- fd (return d)
  return (a', b', c', d')

__BinderMinder_lift_tuple_5__
  :: Monad m
  => (m a -> m a', m b -> m b', m c -> m c', m d -> m d', m e -> m e')
  -> m (a, b, c, d, e)
  -> m (a', b', c', d', e')
__BinderMinder_lift_tuple_5__ (fa, fb, fc, fd, fe) m = do
  (a, b, c, d, e) <- m
  a' <- fa (return a)
  b' <- fb (return b)
  c' <- fc (return c)
  d' <- fd (return d)
  e' <- fe (return e)
  return (a', b', c', d', e')


-- removeImport :: DynFlags -> HsModule GhcPs -> Hsc (HsModule GhcPs)
-- removeImport flags hsMod = do
--   imports <- flip filterM (hsmodImports hsMod) $ \case
--     (L l (ImportDecl { ideclName = L _ importName }))
--       | importName == mkModuleName "BinderMinder" -> do
--           putStrLnHsc $ "[!] removing plugin import from: " ++ showPpr flags l
--           return False
--     _ -> return True
--   return hsMod { hsmodImports = imports }
