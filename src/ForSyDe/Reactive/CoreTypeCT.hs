{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}

module ForSyDe.Reactive.CoreTypeCT where

-- | We represent time as doubles even tough this is not the best
-- representation. For now we are interested in establishing the
-- framework.
type Time = Double

-- | The outputs of our processes will be doubles as well, but it will
-- be better to change latter to the same representation used for
-- Time.
-- newtype Value = Double

-- | Signals are functions over Time.
data SignalCT a = SignalCT (Time -> (a, SignalCT a))

-- | Processes are functions over signals.
data ProcessCT a b = ProcessCT (SignalCT a -> SignalCT b)


-- | API:

-- | 'at' observes a SignalCT at a specified time t.
at :: SignalCT a -> Time -> a
(SignalCT f) `at` t = fst $ f t


-- | Composition operators.
