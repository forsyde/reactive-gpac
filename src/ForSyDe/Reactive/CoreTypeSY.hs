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


-- | Composition operators based on the arrow operators.

-- | 'cascadeSM' is the analog of (.) for state machines. We keep
-- track of the internal states.
cascadeSM :: SM b c s2
          -> SM a b s1
          -> SM a c (s1, s2)
cascadeSM (SM (SF f2) s2) (SM (SF f1) s1) = SM (SF (csf f1 f2)) (s1, s2)
  where
    csf fa fb (sa, sb) a = (c, SM (SF (csf f1n f2n)) (s1n, s2n))
      where
        -- Evaluate first machine with the composition input.
        (b, SM (SF f1n) s1n) = fa sa a
        -- Evaluate second machine with the first output.
        (c, SM (SF f2n) s2n) = fb sb b

-- | Composition operators.
(>>>>) :: SM a b s1
       -> SM b c s2
       -> SM a c (s1, s2)
p1 >>>> p2 = cascadeSM p2 p1

(<<<<) :: SM b c s2
       -> SM a b s1
       -> SM a c (s1, s2)
(<<<<) = cascadeSM

-- | Parallel composition.
firstSM :: SM a b s
        -> SM (a, c) (b, c) s
firstSM (SM (SF sf) s) = SM (SF (fsf sf)) s
  where
    fsf f s (a, c) = ((b,c), SM (SF (fsf sfn)) sn)
      where
        -- Evaluate process.
        (b, SM (SF sfn) sn) = f s a

secondSM :: SM a b s
         -> SM (c, a) (c, b) s
secondSM (SM (SF sf) s) = SM (SF (fsf sf)) s
  where
    fsf f s (c, a) = ((c,b), SM (SF (fsf sfn)) sn)
      where
        -- Evaluate process.
        (b, SM (SF sfn) sn) = f s a

splitSM :: SM a b s1
        -> SM c d s2
        -> SM (a,c) (b,d) (s1,s2)
splitSM (SM (SF f1) s1) (SM (SF f2) s2) = SM (SF (fsf f1 f2)) (s1,s2)
  where
    fsf fa fc (sa, sc) (a,c) = ((b,d), SM (SF (fsf f1n f2n)) (s1n, s2n))
      where
        -- Evaluate first process.
        (b, SM (SF f1n) s1n) = fa sa a
        -- Evaluate second process.
        (d, SM (SF f2n) s2n) = fc sc c

(****) = splitSM

fanoutSM :: SM a b s1
         -> SM a c s2
         -> SM a (b,c) (s1,s2)
fanoutSM (SM (SF f1) s1) (SM (SF f2) s2) = SM (SF (fsf f1 f2)) (s1,s2)
  where
    fsf fa fb (sa, sb) a = ((b,d), SM (SF (fsf f1n f2n)) (s1n, s2n))
      where
        -- Evaluate first process
        (b, SM (SF f1n) s1n) = fa sa a
        -- Evaluate second process
        (d, SM (SF f2n) s2n) = fb sb a

(&&&&) = fanoutSM
