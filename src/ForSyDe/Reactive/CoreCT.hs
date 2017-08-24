module ForSyDe.Reactive.CoreCT where

import ForSyDe.Reactive.CoreTypeCT

-- | Process constructors: we define the GPAC units as process
-- constructors. These are immutable processes in the sense once they
-- are created, their functionality does not change.

-- | Time
time :: SignalCT Time
time = SignalCT (\t -> (t, time))

-- | Constant function
constCT :: a -> SignalCT a
constCT k = SignalCT (\t -> (k, constCT k))

-- | Adder
adderCT :: (Num a) => SignalCT a -> SignalCT a -> SignalCT a
adderCT s1 s2 = SignalCT (\t -> ((s1 `at` t) + (s2 `at` t), adderCT s1 s2))

-- | Multiplier
multCT :: (Num a) => SignalCT a -> SignalCT a -> SignalCT a
multCT s1 s2 = SignalCT (\t -> ((s1 `at` t) * (s2 `at` t), multCT s1 s2))

-- | Integrator


-- | Processes execution

-- | stepCT
stepCT :: SignalCT a -> Time -> (a, SignalCT a)
stepCT s1 t = (s1 `at` t, next s1 t)

-- | execCT
execCT :: SignalCT a -> [Time] -> ([a], SignalCT a)
execCT s1 [] = ([], s1)
execCT s1 (t:ts) = (a:as, finalSig)
  where (a, newS) = stepCT s1 t
        (as, finalSig) = execCT newS ts
