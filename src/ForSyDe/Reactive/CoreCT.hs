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


-- | Integrator
-- | Embedded the trapezoidal rule. Need to find a way to generalize
-- the solver later...
-- | Time in type signature is making me nuts...
--intCT :: (Num a, Num b)
--      => Time                      -- ^ Initial integration time
--      -> b                      -- ^ Initial value
--      -> PCT a b
--      -> PCT a b
intCT t0 y0 (PCT {prCT = p1}) = PCT {prCT = p}
  where
    p t a = (b, intCT t b p')
      where
        b = y0 + (t - t0)/2 * (fa + fb)
        (fa, _)  = p1 t0 a
        (fb, p') = p1 t a
        --p' = (intCT solve t b (PCT {prCT = p1}))


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
stepCT :: PCT () b -> Time -> (b, PCT () b)
stepCT p1 t = (p1 `at` t, nextCT p1 t)

-- -- | execCT
execCT :: PCT () b -> [Time] -> ([b], PCT () b)
execCT p1 [] = ([], p1)
execCT p1 (t:ts) = (b:bs, finalP)
  where (b, newP) = stepCT p1 t
        (bs, finalP) = execCT newP ts
