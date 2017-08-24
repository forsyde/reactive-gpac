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
--data SignalCT a = SignalCT (Time -> (a, SignalCT a))
type SignalCT a = PCT () a

-- | Processes are functions over signals.
data PCT a b = PCT {prCT :: Time -> a -> ContCT a b}

-- | The Continuation data type encapsulates the output of the process
-- and a new Process for later computations.
type ContCT a b = (b, PCT a b)


-- | API:

-- | 'at' observes a PCT at a specified time t.
at :: PCT () a -> Time -> a
p `at` t = fst $ prCT p t ()

--next :: SignalCT a -> Time -> SignalCT a
--next (SignalCT f) t = snd $ f t

-- | Composition operators.
liftCT :: (a -> b) -> PCT a b
liftCT f = PCT {prCT = \_ a -> (f a, liftCT f)}

cascadeCT :: PCT a b
          -> PCT b c
          -> PCT a c
cascadeCT (PCT {prCT = p1}) (PCT {prCT = p2}) =
  PCT {prCT = p}
  where
    p t a = (c, p1' `cascadeCT` p2')
      where
        (b, p1') = p1 t a
        (c, p2') = p2 t b
