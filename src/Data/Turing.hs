-- | This module defines everything required to simulate a simple deterministic
-- Turing machine. There are several equivalent but slightly different
-- definitions of Turing machines in use - ours has the machine write /and/
-- move in each step of execution. In addition, we allow a special movement
-- 'DontMove' that results in just a write operation.
--
-- You just define a transition function using suitable state and input types and run it:
--
-- @
-- turingMachine =
--   let
--       f (q, Just 0) = Just (q, Just 0, MoveRight)
--       f ("even", Just 1) = Just ("odd", Just 0, MoveRight)
--       f ("odd", Just 1) = Just ("even", Just 0, MoveRight)
--       f _ = Nothing
--       t = mkTuringMachine f "even" ["even"] -- a machine that decides parity
--   in run t [0, 0, 1, 1, 1]
-- @
-- >>> turingMachine
-- Reject

module Data.Turing (
    -- * The Turing machine
    TuringMachine
  , mkTuringMachine
    -- * Associated types
  , Movement (..)
  , TransitionFunction
  , MachineResult (..)
    -- * Running a machine
  , run
  , runFor
  ) where

import Data.List (unfoldr, genericTake)


-- | Abstract type representing a Turing machine. It is parameterized over
-- the alphabet and state types. A full definition contains the initial state,
-- a list of accepting states and the transition function. Construct through
-- 'mkTuringMachine'.
data TuringMachine s q = TM q [q] (TransitionFunction s q)

-- | Construct a Turing machine from the transition function, its initial
-- state and a list of accepting states.
mkTuringMachine :: TransitionFunction s q -> q -> [q] -> TuringMachine s q
mkTuringMachine f initState acceptingStates = TM initState acceptingStates f

-- | A Turing machine has three options of moving its read head: left, right,
-- or no move at all.
data Movement = MoveLeft
              | MoveRight
              | DontMove
                deriving (Eq, Show)

-- | The transition function maps the current state and tape contents
-- to a triple describing the new state, the value to write to the tape, and
-- a head movement. If the function is 'Nothing' for the current state
-- and tape contents, the machine halts.
type TransitionFunction s q = (q, Maybe s) -> Maybe (q, Maybe s, Movement)

-- | A machine can either accept or reject the input (for now).
data MachineResult = Accept
                   | Reject
                     deriving (Eq, Show)

-- | This is the machine's execution environment, consisting of the tape
-- contents left and right of the machine's head, as well as the current
-- state.
data MachineEnvironment s q = MS [Maybe s] [Maybe s] q

-- | Run a machine indefinitely on the given input. If it halts, it either
-- accepts or rejects the input.
run :: Eq q => TuringMachine s q -> [s] -> MachineResult
run (TM initState acceptingStates f) input =
    let executionStates = stateSequence initState f input
        terminationState = last executionStates
    in if terminationState `elem` acceptingStates then Accept else Reject

-- | Run a machine for the given number of steps. If this is enough for it to
-- halt, the result is the same as for 'run', wrapped in 'Just'; otherwise it
-- is 'Nothing'.
runFor :: (Integral i, Eq q) => i -> TuringMachine s q -> [s] -> Maybe MachineResult
runFor maxSteps (TM initState acceptingStates f) input =
    let executionStates = genericTake (maxSteps + 2) (stateSequence initState f input)
        terminationState = last executionStates
    in if length executionStates > fromIntegral maxSteps + 1
       then Nothing
       else Just $ if terminationState `elem` acceptingStates then Accept else Reject

-- | Run a single step in the Turing machine's execution and return 'Nothing'
-- it the machine has halted.
step :: TransitionFunction s q -> MachineEnvironment s q -> Maybe (MachineEnvironment s q)
step f (MS  (l:ls) (r:rs) currentState) =
    case f (currentState, r) of
        Nothing -> Nothing
        Just (newState, writeValue, movement) -> case movement of
            MoveLeft -> Just $ MS ls (l:writeValue:rs) newState
            MoveRight -> Just $ MS (writeValue:l:ls) rs newState
            DontMove -> Just $ MS (l:ls) (writeValue:rs) newState

-- | Generate the sequence of states that the a machine with the given initial
-- state and transition function runs through.
stateSequence :: q -> TransitionFunction s q -> [s] -> [q]
stateSequence initState f input = initState : (unfoldr (exec f) initialEnv)
    where initialEnv = MS (repeat Nothing) (map Just input ++ repeat Nothing) initState
          exec f e = case step f e of
            Nothing -> Nothing
            Just ne@(MS _ _ s) -> Just (s, ne)
