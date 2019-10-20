{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Prop where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity


-- | Identifiers
newtype Id = Id { unId :: Int }
  deriving (Show, Eq, Ord)

-- | Propositional values
data Prop =
     Var Id
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
  { ps_variables :: Set Id
  , ps_assumptions :: Map Id Prop
  , ps_goal :: Goal
  , ps_subgoals :: [Goal]
  , ps_uniq :: Int
  } deriving Show

emptyProofState :: ProofState
emptyProofState = ProofState Set.empty Map.empty TRUE [] 0

newVar :: ProofState -> (Prop, ProofState)
newVar ps =
  (Var (Id (ps_uniq ps)),
   ps { ps_uniq = ps_uniq ps + 1
      , ps_variables = Set.insert (Id (ps_uniq ps)) (ps_variables ps) })

newAssumption :: Prop -> ProofState -> (Id, ProofState)
newAssumption prop ps =
  (Id (ps_uniq ps),
   ps { ps_uniq = ps_uniq ps + 1
     , ps_assumptions = Map.insert (Id (ps_uniq ps)) prop (ps_assumptions ps) })

newSubgoal :: Goal -> ProofState -> ProofState
newSubgoal subgoal ps = ps { ps_subgoals = subgoal : ps_subgoals ps }

setGoal :: Goal -> ProofState -> ProofState
setGoal goal ps = ps { ps_goal = goal }

lookupAssumption :: Id -> Proof (Maybe Prop)
lookupAssumption i = Map.lookup i <$> gets ps_assumptions

-- | Proof errors
data ProofError =
    IncompleteProof ProofState
  | InvalidTactic String Goal
  | NoMoreSubgoals String
  deriving Show

-- | The proof type
type Proof = StateT ProofState (ExceptT ProofError Identity)

runProof :: Proof a -> Either ProofError a
runProof = runIdentity . runExceptT . flip evalStateT emptyProofState

runProof' :: Proof a -> IO ()
runProof' p =
  case runProof p of
    Right _ -> putStrLn "No more subgoals"
    Left (InvalidTactic name goal) -> printInvalidTactic name goal
    Left (IncompleteProof ps) -> printIncompleteProof ps
    Left (NoMoreSubgoals name) -> printNoMoreSubgoals name

printInvalidTactic :: String -> Goal -> IO ()
printInvalidTactic name goal = do
  putStrLn $ "Cannot apply tactic <" ++ name ++ "> to the current goal:"
  putStrLn (showProp goal)

printIncompleteProof :: ProofState -> IO ()
printIncompleteProof ps = do
  putStrLn "----------------------------------------"
  putStrLn $ show (length (ps_subgoals ps)) ++ " subgoals:"
  forM_ (Set.elems (ps_variables ps)) $ \(Id i) -> do
    putStrLn $ "V" ++ show i ++ " : Prop"
  forM_ (Map.assocs (ps_assumptions ps)) $ \(Id i, prop) -> do
    putStrLn $ "H" ++ show i ++ " : " ++ showProp prop
  putStrLn "========================================"
  putStrLn $ showProp (head (ps_subgoals ps))
  putStrLn "----------------------------------------"

printNoMoreSubgoals :: String -> IO ()
printNoMoreSubgoals name =
  putStrLn $ "No subgoals left in this branch to apply tactic <" ++ name ++ ">"

showProp :: Prop -> String
showProp = \case
  TRUE -> "TRUE"
  FALSE -> "FALSE"
  Var (Id i) -> "V" ++ show i
  Neg FALSE -> "¬TRUE"
  Neg TRUE -> "¬FALSE"
  Neg prop@(Var {}) -> "¬" ++ showProp prop
  Neg prop -> "¬(" ++ showProp prop ++ ")"
  And x y -> "(" ++ showProp x ++ " /\\ " ++ showProp y ++ ")"
  Or  x y -> "(" ++ showProp x ++ " \\/ " ++ showProp y ++ ")"
  Imp x y -> "(" ++ showProp x ++ " ==> " ++ showProp y ++ ")"
  Eq  x y -> "(" ++ showProp x ++ " <=> " ++ showProp y ++ ")"


-- | Proof basic constuctions
variable :: Proof Prop
variable = state newVar

proof :: Prop -> Proof Prop -> Proof Prop
proof goal body = modify (setGoal goal . newSubgoal goal) >> body

qed :: Proof Prop
qed = do
  ps <- get
  if null (ps_subgoals ps)
    then return (ps_goal ps)
    else throwError (IncompleteProof ps)

-- | Tactics
intro :: Proof Id
intro = gets ps_subgoals >>= \case
  Imp x y : goals ->
    state $ \ps -> newAssumption x (ps { ps_subgoals = y : goals })
  goal : _ -> throwError (InvalidTactic "intro" goal)
  _        -> throwError (NoMoreSubgoals "intro")

intro_ :: Proof ()
intro_ = void intro

exact :: Id -> Proof ()
exact i = do
  ass <- lookupAssumption i
  gets ps_subgoals >>= \case
    g : goals | ass == Just g ->
      modify $ \ps -> ps { ps_subgoals = goals }
    goal : _ -> throwError (InvalidTactic "exact" goal)
    _        -> throwError (NoMoreSubgoals "exact")

assumption :: Proof ()
assumption = do
  assumptions <- Map.elems <$> gets ps_assumptions
  gets ps_subgoals >>= \case
    goal : goals | goal `elem` assumptions ->
      modify $ \ps -> ps { ps_subgoals = goals }
    goal : _ -> throwError (InvalidTactic "assumption" goal)
    _        -> throwError (NoMoreSubgoals "assumption")

split :: Proof () -> Proof ()
split body = gets ps_subgoals >>= \case
  And x y : goals -> do
    assumptions <- gets ps_assumptions
    modify $ \ps -> ps { ps_subgoals = [x, y] }
    body
    void qed
    modify $ \ps -> ps { ps_subgoals = goals, ps_assumptions = assumptions }
  goal : _ -> throwError (InvalidTactic "split" goal)
  _        -> throwError (NoMoreSubgoals "split")

left :: Proof () -> Proof ()
left body = gets ps_subgoals >>= \case
  Or x _ : goals -> do
    assumptions <- gets ps_assumptions
    modify $ \ps -> ps { ps_subgoals = [x] }
    body
    void qed
    modify $ \ps -> ps { ps_subgoals = goals, ps_assumptions = assumptions }
  goal : _ -> throwError (InvalidTactic "left" goal)
  _        -> throwError (NoMoreSubgoals "left")

right :: Proof () -> Proof ()
right body = gets ps_subgoals >>= \case
  Or _ y : goals -> do
    assumptions <- gets ps_assumptions
    modify $ \ps -> ps { ps_subgoals = [y] }
    body
    void qed
    modify $ \ps -> ps { ps_subgoals = goals, ps_assumptions = assumptions }
  goal : _ -> throwError (InvalidTactic "right" goal)
  _        -> throwError (NoMoreSubgoals "right")


-- | Tests

test :: Proof Prop
test = do

  p <- variable
  q <- variable

  proof (p ==> q ==> p \/ q) do
    intro
    intro
    left do
      assumption
    qed
