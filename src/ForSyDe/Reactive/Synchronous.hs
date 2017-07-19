{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}

module ForSyDe.Reactive.Synchronous where

-- | The basic process is the mealy machine and all other process
-- constructors are derived from it. In this way, I can consider only
-- one case for processes composition.

-- | 'SM' represents a state machine with input of type @a@, output of
-- type @b@ and an internal state of type @s@. 'SF' represents a @state function@ that merges the @next state function@ and the @output decoder@ for a mealy machine.
data SM a b s = SM (SF a b s) s

-- | 'SF' represents a @state function@. It takes the inputs and the
-- internal state to generate a new state and the outputs. This is
-- inspiered on the idea of having a process to generate an output and
-- a new process to be used next.
newtype SF a b s = SF (s -> a -> (b, SM a b s))


-- | API: we define two utility functions. 'state' returns the internal state of a 'SM' and 'sFunction' returns the @state function@.

-- | 'state' reads the internal state of a 'SM'.
state :: SM a b s -> s
state (SM _ s) = s

-- | 'sFunction' returns the state function of a 'SM'.
sFunction :: SM a b s -> (s -> a -> (b, SM a b s))
sFunction (SM (SF sf) _) = sf


-- | Process constructors: we define the usual ForSyDe process
-- constructors. These are immutable processes in the sense once they are created, their functionality does not change.

-- | 'mealySY' builds a mealy machine process.
mealySY :: (s -> a -> s)        -- ^ Next state decoder.
        -> (s -> a -> b)        -- ^ Output decoder.
        -> s                    -- ^ Initial state.
        -> SM a b s             -- ^ Mealy state machine process.
mealySY nsf dec s0 = SM (st) s0
  where st = SF (\s a -> (dec s a, SM (st) (nsf s a)))

-- | 'mooreSY' builds a moore machine process.
mooreSY :: (s -> a -> s)        -- ^ Next state decoder.
        -> (s -> b)             -- ^ Output decoder.
        -> s                    -- ^ Initial state.
        -> SM a b s             -- ^ Moore state machine process.
mooreSY nsf dec s0 = SM (st) s0
  where st = SF (\s a -> (dec s, SM (st) (nsf s a)))

-- | 'delaySY' delays the input by one event cycle by introducing an
-- initial value at the beginning of the output signal.
delaySY :: s                    -- ^ Initial state.
        -> SM s s s             -- ^ Delay process.
delaySY s0 = SM (sf) s0
  where sf = SF (\s a -> (s, SM (sf) a))

-- | 'mapSY' builds a combinational process.
mapSY :: (a -> b)               -- ^ Combinational function.
      -> SM a b ()              -- ^ Combinational process.
mapSY f = SM (sf) ()
  where sf = SF (\s a -> (f a, SM (sf) ()))


-- | Mutable (reconfigurable) processes?

-- | Class instances
-- | 'Show' instance: we decide to show only the internal state of the
-- machine.
instance (Show s) => Show (SM a b s) where
  show (SM sf s) = show s
