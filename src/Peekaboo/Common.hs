{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
module Peekaboo.Common where

import Control.Monad
import Data.Generics (mkM, everywhereM, listify)

import FastString as FS
import HsSyn      as GHC
import GhcPlugins as GHC
import OccName    as Name

----------------------------------------
-- | The plugin itself, parameterized by the annotating function

peekaboo :: RdrName -> [CommandLineOption] -> HsParsedModule -> Hsc HsParsedModule
peekaboo ann_fun opts parsed = do
  message $ "starting plugin"

  flags <- getDynFlags
  let L loc hsMod = hpm_module parsed

  -- create an annotation token based on the plugin options (|$| by default)
  let ann_tok = case opts of [sym] -> mkRdrName sym; _ -> mkRdrName "|$|"

  -- extract all the annotation pragmas from the code
  let anns = extractAnn <$> listify (isAnn flags) hsMod

  let transform =
        everywhereM (mkM (annotateTokens       flags ann_fun ann_tok)) >=>
        everywhereM (mkM (annotateTopLevelAnns flags ann_fun anns))

  hsMod' <- transform hsMod

  message $ "[!] done"
  return parsed { hpm_module = L loc hsMod' }

----------------------------------------
-- | Search for top-level bindings of the shape:
--
-- {-# ANN foo SrcInfo #-}
-- foo = do ...
--
annotateTopLevelAnns
  :: DynFlags
  -> RdrName
  -> [RdrName]
  -> Match GhcPs (LHsExpr GhcPs)
  -> Hsc (Match GhcPs (LHsExpr GhcPs))
annotateTopLevelAnns flags ann_fun anns = \case

  -- annotate match statements that appear in the module annotations
  Match m_x m_ctx m_ps
    (GRHSs grhss_x
      [L l (GRHS grhs_x grhs_guards
             (L l' (HsDo do_x do_cxt (L l'' doStmts))))] lbs)
    | unLoc (mc_fun m_ctx) `elem` anns -> do
        message $ "annotating do expression at " ++ showPpr flags l

        doStmts' <- mapM (annotateStmt flags ann_fun) doStmts
        return (Match m_x m_ctx m_ps
                 (GRHSs grhss_x
                   [L l (GRHS grhs_x grhs_guards
                          (L l' (HsDo do_x do_cxt (L l'' doStmts'))))] lbs))

  -- otherwise, just return the match unchanged
  match -> return match

----------------------------------------
-- | Search for annotated do expressions of the shape:
--
-- some expression <ann_tok> do ...
--
annotateTokens
  :: DynFlags
  -> RdrName
  -> RdrName
  -> LHsExpr GhcPs
  -> Hsc (LHsExpr GhcPs)
annotateTokens flags ann_fun ann_tok = \case

  -- annotate do expression prefixed with the annotation token
  L l (OpApp _
        lhs
        (L _ (HsVar _   (L _ tok)))
        (L _ (HsDo  _ y (L z doStmts))))
    | showPpr flags tok == showPpr flags ann_tok -> do
        message $ "annotating do expression at " ++ showPpr flags l

        doStmts' <- mapM (annotateStmt flags ann_fun) doStmts
        let do' = L l (HsDo noExt y (L z doStmts'))
        let expr' = paren lhs & paren do'

        return expr'

  -- otherwise, return the expression unchanged
  expr -> return expr

----------------------------------------
-- | Annotate a single do statement
annotateStmt
  :: DynFlags
  -> RdrName
  -> ExprLStmt GhcPs
  -> Hsc (ExprLStmt GhcPs)
annotateStmt flags ann_fun = \case

  -- bind statements where the lhs is a variable pattern
  L l (BindStmt x pat@(L _ (VarPat _ (L _ bind))) body y z) -> do

    let body' =
          var ann_fun
          & paren (var peekabooSrcInfo
                   & paren (var peekabooJust
                            & varPatToLitStr flags pat)
                   & paren (mkLocExpr l))
          & paren body

    message $ "  found single bind ("++ render bind ++ ") at " ++ render l
    return (L l (BindStmt x pat body' y z))

  -- bind statements where the lhs is a tuple pattern
  L l (BindStmt x pat@(L _ (TuplePat _ binds _)) body y z)
    | length binds > 1 && length binds <= 5 && all isVarPat binds -> do

        let bindStrs = varPatToLitStr flags <$> binds
        let body' =
              var (peekabooLifter (length binds))
              & mkAnnTuple ann_fun l bindStrs
              & paren body

        message $ "  found tuple bind ("++ render pat ++ ") at " ++ render l
        return (L l (BindStmt x pat body' y z))

  -- body statements
  L l (BodyStmt x body y z) -> do

    let body' =
          var ann_fun
          & paren (var peekabooSrcInfo
                   & var peekabooNothing
                   & paren (mkLocExpr l))
          & paren body

    message $ "  found body statement at " ++ render l
    return (L l (BodyStmt x body' y z))

  -- everything else is left unchanged
  stmt -> return stmt

  where render :: forall a. Outputable a => a -> String
        render = showPpr flags


----------------------------------------
-- | Helper functions

-- | Print a message to the console
message :: String -> Hsc ()
message str = liftIO $ putStrLn $ "[Peekaboo] " ++ str

-- | Create a name from a string
mkRdrName :: String -> RdrName
mkRdrName = mkUnqual Name.varName . mkFastString

-- | Create a Loc tuple from a SrcSpan
mkLocExpr :: SrcSpan -> LHsExpr GhcPs
mkLocExpr (UnhelpfulSpan {}) =
  var peekabooNothing
mkLocExpr (RealSrcSpan loc) =
  paren (var peekabooJust &
         tuple [ strLit (srcSpanFile loc)
               , numLit (srcSpanStartLine loc)
               , numLit (srcSpanStartCol loc) ])

-- | Create a tuple of the shape:
-- (ann_fun name1 loc, ann_fun name2, ...)
mkAnnTuple :: RdrName -> SrcSpan -> [LHsExpr GhcPs] -> LHsExpr GhcPs
mkAnnTuple ann_fun loc bindStrs =
  tuple (mkElem <$> bindStrs)
  where mkElem bindStrLit =
         var ann_fun
         & paren (var peekabooSrcInfo
                  & paren (var peekabooJust & bindStrLit)
                  & paren (mkLocExpr loc))

-- | Transform a variable pattern into its corresponding string expression
varPatToLitStr :: DynFlags -> LPat GhcPs -> LHsExpr GhcPs
varPatToLitStr flags (L _ (VarPat _ name)) = strLit (fsLit (showPpr flags name))
varPatToLitStr _     _                     = error "this should not happen"


-- | Is this pattern a variable?
isVarPat :: LPat GhcPs -> Bool
isVarPat (L _ (VarPat {})) = True
isVarPat _                 =  False

-- | Check whether an annotation pragma is of the shape:
-- | {-# ANN ident SrcInfo #-}
pattern HsAnn :: RdrName -> RdrName -> AnnDecl GhcPs
pattern HsAnn lhs rhs <-
  HsAnnotation _ _
  (ValueAnnProvenance (L _ lhs))
  (L _ (HsVar _ (L _ rhs)))

isAnn :: DynFlags -> AnnDecl GhcPs -> Bool
isAnn flags (HsAnn _ rhs) = showPpr flags rhs == showPpr flags peekabooSrcInfoTag
isAnn _     _             = False

extractAnn :: AnnDecl GhcPs -> RdrName
extractAnn (HsAnn target _) = target
extractAnn _                = error "this should not happen"


----------------------------------------
-- | Located expressions builder interface

app :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
app x y = noLoc (HsApp noExt x y)

(&) :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
(&) = app
infixl 4 &

paren :: LHsExpr GhcPs -> LHsExpr GhcPs
paren x = noLoc (HsPar noExt x)

var :: RdrName -> LHsExpr GhcPs
var x = noLoc (HsVar noExt (noLoc x))

numLit :: Int -> LHsExpr GhcPs
numLit n = noLoc (HsLit noExt (HsInt noExt (mkIntegralLit n)))

strLit :: FastString -> LHsExpr GhcPs
strLit s = noLoc (HsLit noExt (HsString NoSourceText s))

tuple :: [LHsExpr GhcPs] -> LHsExpr GhcPs
tuple exprs = noLoc (ExplicitTuple noExt (noLoc . Present noExt <$> exprs) Boxed)


----------------------------------------
-- | Name wrappers

-- | Wrapped names
peekabooSrcInfoTag :: RdrName
peekabooSrcInfoTag = mkRdrName "SrcInfo"

peekabooJust :: RdrName
peekabooJust = mkRdrName "__Peekaboo_Just__"

peekabooNothing :: RdrName
peekabooNothing = mkRdrName "__Peekaboo_Nothing__"

peekabooSrcInfo :: RdrName
peekabooSrcInfo = mkRdrName "__Peekaboo_SrcInfo__"

peekabooAnnotateMPure :: RdrName
peekabooAnnotateMPure = mkRdrName "__Peekaboo_Data_Annotated_annotateM__"

peekabooAnnotateMMonadic :: RdrName
peekabooAnnotateMMonadic = mkRdrName "__Peekaboo_Data_Annotated_Monadic_annotateM__"

peekabooAnnotateMTransformer :: RdrName
peekabooAnnotateMTransformer = mkRdrName "__Peekaboo_Control_Monad_Annotated_annotateM__"

peekabooLifter :: Int -> RdrName
peekabooLifter n = mkRdrName $ "__Peekaboo_lift_tuple_"++ show n ++"__"
