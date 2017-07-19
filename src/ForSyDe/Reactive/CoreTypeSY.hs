{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}

module ForSyDe.Reactive.CoreTypeSY where

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

-- | 'CM' representes a state machine with unacessable internal state
-- or a combinational process.
type CB a b = SM a b ()


-- | API: we define two utility functions. 'state' returns the internal state of a 'SM' and 'sFunction' returns the @state function@.

-- | 'state' reads the internal state of a 'SM'.
state :: SM a b s -> s
state (SM _ s) = s

-- | 'sFunction' returns the state function of a 'SM'.
sFunction :: SM a b s -> (s -> a -> (b, SM a b s))
sFunction (SM (SF sf) _) = sf


-- | Class instances
-- | 'Show' instance: we decide to show only the internal state of the
-- machine.
instance (Show s) => Show (SM a b s) where
  show (SM sf s) = show s
