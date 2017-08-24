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
