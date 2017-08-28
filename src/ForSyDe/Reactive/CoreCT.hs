module ForSyDe.Reactive.CoreCT where

import ForSyDe.Reactive.CoreTypeCT

-- | Process constructors: we define the GPAC units as process
-- constructors. These are immutable processes in the sense once they
-- are created, their functionality does not change.

-- | Time
time :: PCT () Time
time = PCT (\t () -> (t, time))

-- | Constant function
constCT :: a -> PCT () a
constCT k = PCT (\_ () -> (k, constCT k))

-- | Null function. Ugly but I still could not think of how to avoid this...
nullCT :: PCT () ()
nullCT = PCT (\t _ -> ((), nullCT))

-- | Adder
adderCT :: (Num b) => PCT a (b, b) -> PCT a b
adderCT (PCT {prCT = p1}) = PCT {prCT = p}
  where
    p t a = (uncurry (+) c, adderCT p1')
      where
        (c, p1') = p1 t a

-- | Multiplier
multCT :: (Num b) => PCT a (b, b) -> PCT a b
multCT (PCT {prCT = p1}) = PCT {prCT = p}
  where
    p t a = (uncurry (*) c, adderCT p1')
      where
        (c, p1') = p1 t a


-- | Multiplier
--multCT :: (Num a) => SignalCT a -> SignalCT a -> SignalCT a
--multCT s1 s2 = SignalCT (\t -> ((s1 `at` t) * (s2 `at` t), multCT s1 s2))

-- | Integrator
-- ???

-- | Solver: solver function for the integral. In the case of integral
-- only, RK4 reduces to Simpson's rule.
--type Value = Time
--rk4 :: Time                     -- ^ Initial time (t0)
--     -> Value                    -- ^ Initial vale (y0)
--     -> (Time -> Value)          -- ^ Integrand function
--     -> Time                     -- ^ Time of interest
--     -> Value                    -- ^ Integral value
-- rk4 t0 y0 f t =
--   let h = t - t0
--       k1 = f t0
--       k2 = f (t0 + h/2)
--       k3 = f t
--   in y0 + h/6 * (k1 + 4 * k2 + k3)


-- -- | Processes execution

-- -- | stepCT
-- stepCT :: SignalCT a -> Time -> (a, SignalCT a)
-- stepCT s1 t = (s1 `at` t, next s1 t)

-- -- | execCT
-- execCT :: SignalCT a -> [Time] -> ([a], SignalCT a)
-- execCT s1 [] = ([], s1)
-- execCT s1 (t:ts) = (a:as, finalSig)
--   where (a, newS) = stepCT s1 t
--         (as, finalSig) = execCT newS ts
