{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PatternSynonyms #-}

module BinderMinder
  ( BinderMinder.Name
  , BinderMinder.Loc
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

type Name = String
type Loc  = (FilePath, Int, Int) -- file, line and column

type Anns = [(RdrName, RdrName)]

----------------------------------------
-- The plugin itself
--

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = \opts _summ -> binderMinder opts }
  where
    binderMinder opts parsed = do
      message $ "starting plugin"

      flags <- getDynFlags
      let L loc hsMod = hpm_module parsed

      -- create an annotation token based on the plugin options (@@ by default)
      let ann_tok = case opts of [sym] -> mkRdrName sym; _ -> mkRdrName "@@"

      -- extract all the annotation pragmas from the code
      let anns = extractAnn <$> listify isAnn hsMod

      let transform = everywhereM (mkM (annotateTokens       flags ann_tok))
                  >=> everywhereM (mkM (annotateTopLevelAnns flags anns))

      hsMod' <- transform hsMod

      message $ "[!] done"
      return parsed { hpm_module = L loc hsMod' }


----------------------------------------
-- | Search for top-level bindings of the shape:
-- foo = do ...
-- where foo has an ANN annotation somewhere in the code

annotateTopLevelAnns :: DynFlags -> Anns -> Match GhcPs (LHsExpr GhcPs)
                     -> Hsc (Match GhcPs (LHsExpr GhcPs))
annotateTopLevelAnns flags anns = \case

  -- annotate match statements that appear in the module annotations
  Match m_x m_ctx m_ps
    (GRHSs grhss_x
      [L l (GRHS grhs_x grhs_guards
             (L l' (HsDo do_x do_cxt (L l'' doStmts))))] lbs)
    | Just ann_fun <- lookup (unLoc (mc_fun m_ctx)) anns -> do
        message $ "annotating do expression at " ++ showPpr flags l

        doStmts' <- mapM (annotateDoStmt flags ann_fun) doStmts
        return (Match m_x m_ctx m_ps
                 (GRHSs grhss_x
                   [L l (GRHS grhs_x grhs_guards
                          (L l' (HsDo do_x do_cxt (L l'' doStmts'))))] lbs))

  -- otherwise, just return the match unchanged
  match -> return match

----------------------------------------
-- | Search for annotated do expressions of the shape:
-- `foo = ann_fun <ann_tok> do ...`

annotateTokens :: DynFlags -> RdrName -> LHsExpr GhcPs -> Hsc (LHsExpr GhcPs)
annotateTokens flags ann_tok = \case
  -- annotate do expression prefixed with the annotation token
  L l (OpApp _
        (L _ (HsVar _   (L _ ann_fun)))
        (L _ (HsVar _   (L _ tok)))
        (L _ (HsDo  _ y (L z doStmts))))
    | showPpr flags tok == showPpr flags ann_tok -> do
        message $ "annotating do expression at " ++ showPpr flags l

        doStmts' <- mapM (annotateDoStmt flags ann_fun) doStmts
        return (L l (HsDo noExt y (L z doStmts')))

  -- otherwise, just return the expression unchanged
  expr -> return expr


----------------------------------------
-- | Annotate a do statement

annotateDoStmt :: DynFlags -> RdrName -> ExprLStmt GhcPs -> Hsc (ExprLStmt GhcPs)
annotateDoStmt flags ann_fun = \case

  -- Bind statements where the lhs is a variable pattern
  L l (BindStmt x pat@(L _ (VarPat _ (L _ bind))) body y z) -> do

    let body' =
          var ann_fun
          `app` paren (var pluginJust `app` varPatToLitStr flags pat)
          `app` paren (mkLocExpr l)
          `app` paren body

    message $ "  found single bind ("++ pretty bind ++ ") at " ++ pretty l
    return (L l (BindStmt x pat body' y z))

  -- Bind statements where the lhs is a tuple pattern
  L l (BindStmt x pat@(L _ (TuplePat _ binds _)) body y z)
    | length binds > 1 && length binds <= 5 && all isVarPat binds -> do

        let body' =
              var (pluginLifter (length binds))
              `app` mkAnnTuple ann_fun l (varPatToLitStr flags <$> binds)
              `app` paren body

        message $ "  found tuple bind ("++ pretty pat ++ ") at " ++ pretty l
        return (L l (BindStmt x pat body' y z))

  -- Body statements
  L l (BodyStmt x body y z) -> do

    let body' =
          var ann_fun
          `app` var pluginNothing
          `app` paren (mkLocExpr l)
          `app` paren body

    message $ "  found body statement at " ++ pretty l
    return (L l (BodyStmt x body' y z))

  -- Everything else is left unchanged
  stmt -> return stmt

  where
    pretty :: forall a. Outputable a => a -> String
    pretty = showPpr flags


----------------------------------------
-- | Helper functions

-- | Located expressions builder interface
app :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
app x y = noLoc (HsApp noExt x y)

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


-- | Exported names
pluginJust :: RdrName
pluginJust = mkRdrName "__BinderMinder_Just__"

pluginNothing :: RdrName
pluginNothing = mkRdrName "__BinderMinder_Nothing__"

pluginLifter :: Int -> RdrName
pluginLifter n = mkRdrName $ "__BinderMinder_lift_tuple_"++ show n ++"__"


-- | Create a name from a string
mkRdrName :: String -> RdrName
mkRdrName = mkUnqual Name.varName . mkFastString


-- | Transform a variable pattern into its corresponding string expression
varPatToLitStr :: DynFlags -> LPat GhcPs -> LHsExpr GhcPs
varPatToLitStr flags (L _ (VarPat _ name)) = strLit (fsLit (showPpr flags name))
varPatToLitStr _     _                     = error "this should not happen"

-- | Create a tuple of the shape:
-- (ann_fun name1 loc, ann_fun name2, ...)
mkAnnTuple :: RdrName -> SrcSpan -> [LHsExpr GhcPs] -> LHsExpr GhcPs
mkAnnTuple ann_fun loc bindStrs =
  tuple (mkElem <$> bindStrs)
  where mkElem bindStrLit =
          var ann_fun
          `app` paren (var pluginJust `app` bindStrLit)
          `app` paren (mkLocExpr loc)

-- | Create a Loc tuple from a SrcSpan
mkLocExpr :: SrcSpan -> LHsExpr GhcPs
mkLocExpr (UnhelpfulSpan {}) =
  var pluginNothing
mkLocExpr (RealSrcSpan loc) =
  var pluginJust
  `app` tuple [ strLit (srcSpanFile loc)
              , numLit (srcSpanStartLine loc)
              , numLit (srcSpanStartCol loc) ]

-- | Is this pattern a variable?
isVarPat :: LPat GhcPs -> Bool
isVarPat (L _ (VarPat {})) = True
isVarPat _                 =  False

-- | Check whether an annotation pragma is of the shape:
-- | {-# ANN ident "str" #-}

pattern Ann :: RdrName -> FastString -> AnnDecl GhcPs
pattern Ann target ann_fun <-
  HsAnnotation _ _
  (ValueAnnProvenance (L _ target))
  (L _ (HsLit _ (HsString _ ann_fun)))

isAnn :: AnnDecl GhcPs -> Bool
isAnn (Ann {}) = True
isAnn _        = False

extractAnn :: AnnDecl GhcPs -> (RdrName, RdrName)
extractAnn (Ann target ann_fun) = (target, mkVarUnqual ann_fun)
extractAnn _                    =  error "this should not happen"

-- | Print a message to the console
message :: String -> Hsc ()
message str = liftIO $ putStrLn $ "[BinderMinder] " ++ str

----------------------------------------
-- | Exported functions

-- | Make the default annotation token an actual function.
-- This way we can at least raise a warning or something
(@@) :: a -> b -> b
(@@) _ = trace "*** warning: you are using @@ but the plugin is not enabled!"
infix 2 @@

-- | Wrap Nothing and Just under a reasonably unique variable name
__BinderMinder_Nothing__ :: Maybe a
__BinderMinder_Nothing__ = Nothing

__BinderMinder_Just__ :: a -> Maybe a
__BinderMinder_Just__ = Just

-- | Lift annotation functions to monadic values returning tuples
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
