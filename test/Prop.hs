{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{-# OPTIONS_GHC
  -fplugin     Peekaboo.Monadic
  -fplugin-opt Peekaboo.Monadic::=
#-}

module Prop where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity

import Peekaboo.Monadic
import Data.Annotated.Monadic


-- | Identifiers
newtype Var = MkVar { unVar :: Int }
  deriving (Show, Eq, Ord)

newtype Hyp = MkHyp { unHyp :: Int }
  deriving (Show, Eq, Ord)


-- | Propositional values
data Prop =
     Var Var
   | Neg Prop
   | And Prop Prop
   | Or  Prop Prop
   | Imp Prop Prop
   | Eq  Prop Prop
   | TRUE | FALSE
   deriving (Show, Eq)

neg :: Prop -> Prop
neg = Neg

(/\) :: Prop -> Prop -> Prop
(/\) = And
infixl 7 /\

(\/) :: Prop -> Prop -> Prop
(\/) = Or
infixl 6 \/

(==>) :: Prop -> Prop -> Prop
(==>) = Imp
infixr 5 ==>

(<=>) :: Prop -> Prop -> Prop
(<=>) = Eq
infix 4 <=>


-- | Proof state
type Goal = Prop

data ProofState = ProofState
  { ps_variables :: Set Var
  , ps_assumptions :: Map Hyp Prop
  , ps_goal :: Goal
  , ps_subgoals :: [Goal]
  , ps_uniq :: Int
  , ps_ann_vars :: Map Var SrcInfo
  , ps_ann_hyps :: Map Hyp SrcInfo
  , ps_ann_last :: Maybe SrcInfo
  } deriving Show

emptyProofState :: ProofState
emptyProofState = ProofState Set.empty Map.empty TRUE [] 0 Map.empty Map.empty Nothing

newVar :: ProofState -> (Prop, ProofState)
newVar ps =
  (Var (MkVar (ps_uniq ps)),
   ps { ps_uniq = ps_uniq ps + 1
      , ps_variables = Set.insert (MkVar (ps_uniq ps)) (ps_variables ps) })

newAssumption :: Prop -> ProofState -> (Hyp, ProofState)
newAssumption prop ps =
  (MkHyp (ps_uniq ps),
   ps { ps_uniq = ps_uniq ps + 1
     , ps_assumptions = Map.insert (MkHyp (ps_uniq ps)) prop (ps_assumptions ps) })

newSubgoal :: Goal -> ProofState -> ProofState
newSubgoal subgoal ps = ps { ps_subgoals = subgoal : ps_subgoals ps }

setGoal :: Goal -> ProofState -> ProofState
setGoal goal ps = ps { ps_goal = goal }

lookupAssumption :: Hyp -> Proof (Maybe Prop)
lookupAssumption i = Map.lookup i <$> gets ps_assumptions


insertVarSrcInfo :: Var -> SrcInfo -> Proof ()
insertVarSrcInfo i info = modify $ \ps ->
  ps { ps_ann_vars = Map.insert i info (ps_ann_vars ps) }

insertHypSrcInfo :: Hyp -> SrcInfo -> Proof ()
insertHypSrcInfo h info = modify $ \ps ->
  ps { ps_ann_hyps = Map.insert h info (ps_ann_hyps ps) }

updateLastTacticInfo :: SrcInfo -> Proof ()
updateLastTacticInfo info = modify $ \ps -> ps { ps_ann_last = Just info }



-- | Proof errors
data ProofError =
    IncompleteProof ProofState
  | InvalidTactic   ProofState String Goal
  | NoMoreSubgoals  ProofState String
  deriving Show

-- | The proof type
type Proof = StateT ProofState (ExceptT ProofError IO)

instance Annotated SrcInfo Proof Prop where
  annotateM info pp = do
    p <- pp
    case p of
      Var v -> insertVarSrcInfo v info >> return p
      _     -> return p

instance Annotated SrcInfo Proof Hyp where
  annotateM info ph = do
    h <- ph
    insertHypSrcInfo h info
    return h

instance Annotated SrcInfo Proof () where
  annotateM info pu = do
    () <- pu
    updateLastTacticInfo info
    return ()


runProof :: Proof a -> IO (Either ProofError a)
runProof = runExceptT . flip evalStateT emptyProofState

runProofInteractive :: Proof a -> IO ()
runProofInteractive p =
  runProof p >>= \case
    Right _ -> putStrLn "No more subgoals"
    Left (InvalidTactic ps name goal) -> printInvalidTactic ps name goal
    Left (IncompleteProof ps) -> printIncompleteProof ps
    Left (NoMoreSubgoals ps name) -> printNoMoreSubgoals name

printInvalidTactic :: ProofState -> String -> Goal -> IO ()
printInvalidTactic ps name goal = do
  case ps_ann_last ps of
    Just (MkSrcInfo _ (Just (f,r,c))) -> putStrLn $ "Error at: " ++ f ++ ":" ++ show (r,c)
    _ -> return ()
  putStrLn $ "Cannot apply tactic <" ++ name ++ "> to the current goal:"
  putStrLn (showProp ps goal)


printIncompleteProof :: ProofState -> IO ()
printIncompleteProof ps = do
  putStrLn "----------------------------------------"
  putStrLn $ show (length (ps_subgoals ps)) ++ " subgoals:"
  forM_ (Set.elems (ps_variables ps)) $ \i -> do
    putStrLn $ showVar ps i ++ " : Prop"
  forM_ (Map.assocs (ps_assumptions ps)) $ \(h, prop) -> do
    putStrLn $ showHyp ps h ++ " : " ++ showProp ps prop
  putStrLn "========================================"
  putStrLn $ showProp ps (head (ps_subgoals ps))
  putStrLn "----------------------------------------"

showVar :: ProofState -> Var -> String
showVar ps i
  | Just (MkSrcInfo (Just name) _) <- Map.lookup i (ps_ann_vars ps) = name
  | otherwise = "v_" ++ show (unVar i)

showHyp :: ProofState -> Hyp -> String
showHyp ps h
  | Just (MkSrcInfo (Just name) _) <- Map.lookup h (ps_ann_hyps ps) = name
  | otherwise = "H" ++ show (unHyp h)

printNoMoreSubgoals :: String -> IO ()
printNoMoreSubgoals name =
  putStrLn $ "No subgoals left in this branch to apply tactic <" ++ name ++ ">"

showProp :: ProofState -> Prop -> String
showProp ps = \case
  TRUE -> "TRUE"
  FALSE -> "FALSE"
  Var v -> showVar ps v
  Neg FALSE -> "¬TRUE"
  Neg TRUE -> "¬FALSE"
  Neg prop@(Var {}) -> "¬" ++ showProp ps prop
  Neg prop -> "¬(" ++ showProp ps prop ++ ")"
  And x y -> "(" ++ showProp ps x ++ " /\\ " ++ showProp ps y ++ ")"
  Or  x y -> "(" ++ showProp ps x ++ " \\/ " ++ showProp ps y ++ ")"
  Imp x y -> "(" ++ showProp ps x ++ " ==> " ++ showProp ps y ++ ")"
  Eq  x y -> "(" ++ showProp ps x ++ " <=> " ++ showProp ps y ++ ")"





-- | Proof basic constuctions
var :: Proof Prop
var = state newVar

class Variables a where
  vars :: Proof a

instance Variables (Prop, Prop) where
  vars = (,) <$> var <*> var

instance Variables (Prop, Prop, Prop) where
  vars = (,,) <$> var <*> var <*> var


proof :: Prop -> Proof Prop -> Proof Prop
proof goal body = modify (setGoal goal . newSubgoal goal) >> body

qed :: Proof Prop
qed = do
  ps <- get
  if null (ps_subgoals ps)
    then return (ps_goal ps)
    else throwError (IncompleteProof ps)



-- | Tactics
intro :: Proof Hyp
intro = gets ps_subgoals >>= \case
  Imp x y : goals ->
    state $ \ps -> newAssumption x (ps { ps_subgoals = y : goals })
  goal : _ -> get >>= \ps -> throwError (InvalidTactic ps "intro" goal)
  _        -> get >>= \ps -> throwError (NoMoreSubgoals ps "intro")

intro_ :: Proof ()
intro_ = void intro

exact :: Hyp -> Proof ()
exact i = do
  ass <- lookupAssumption i
  gets ps_subgoals >>= \case
    g : goals | ass == Just g ->
      modify $ \ps -> ps { ps_subgoals = goals }
    goal : _ -> get >>= \ps -> throwError (InvalidTactic ps "exact" goal)
    _        -> get >>= \ps -> throwError (NoMoreSubgoals ps "exact")

assumption :: Proof ()
assumption = do
  assumptions <- Map.elems <$> gets ps_assumptions
  gets ps_subgoals >>= \case
    goal : goals | goal `elem` assumptions ->
      modify $ \ps -> ps { ps_subgoals = goals }
    goal : _ -> get >>= \ps -> throwError (InvalidTactic ps "assumption" goal)
    _        -> get >>= \ps -> throwError (NoMoreSubgoals ps "assumption")

split :: Proof () -> Proof ()
split body = gets ps_subgoals >>= \case
  And x y : goals -> do
    assumptions <- gets ps_assumptions
    modify $ \ps -> ps { ps_subgoals = [x, y] }
    body
    void qed
    modify $ \ps -> ps { ps_subgoals = goals, ps_assumptions = assumptions }
  goal : _ -> get >>= \ps -> throwError (InvalidTactic ps "split" goal)
  _        -> get >>= \ps -> throwError (NoMoreSubgoals ps "split")

left :: Proof () -> Proof ()
left body = gets ps_subgoals >>= \case
  Or x _ : goals -> do
    assumptions <- gets ps_assumptions
    modify $ \ps -> ps { ps_subgoals = [x] }
    body
    void qed
    modify $ \ps -> ps { ps_subgoals = goals, ps_assumptions = assumptions }
  goal : _ -> get >>= \ps -> throwError (InvalidTactic ps "left" goal)
  _        -> get >>= \ps -> throwError (NoMoreSubgoals ps "left")

right :: Proof () -> Proof ()
right body = gets ps_subgoals >>= \case
  Or _ y : goals -> do
    assumptions <- gets ps_assumptions
    modify $ \ps -> ps { ps_subgoals = [y] }
    body
    void qed
    modify $ \ps -> ps { ps_subgoals = goals, ps_assumptions = assumptions }
  goal : _ -> get >>= \ps -> throwError (InvalidTactic ps "right" goal)
  _        -> get >>= \ps -> throwError (NoMoreSubgoals ps "right")


















----------------------------------------
-- | Tests

main = runProofInteractive := do

  (p, q) <- vars

  proof (p ==> q ==> p /\ q) := do
    h1 <- intro 
    h2 <- intro

    split := do
      -- void qed
      exact h1
      exact h1

    qed








