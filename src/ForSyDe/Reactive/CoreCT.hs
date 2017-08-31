module ForSyDe.Reactive.CoreCT where

import ForSyDe.Reactive.CoreTypeCT

-- | Process constructors: we define the GPAC units as process
-- constructors. These are immutable processes in the sense once they
-- are created, their functionality does not change.

-- | Time
time :: PCT a Time
time = PCT (\t _ -> (t, time))

-- | Constant function
constCT :: b -> PCT a b
constCT k = PCT (\_ _ -> (k, constCT k))

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
--intCT t0 y0 (PCT {prCT = p1}) = PCT {prCT = p}
--  where
--    p t a = (b, intCT t b p')
--      where
--        b = y0 + (t - t0) * (fa + fb)/2
--        (fa, (PCT {prCT = p1'})) = p1 t0 a
--        (fb, p')  = p1' t a

intCT solver t0 y0 p1 = PCT {prCT = p}
  where
    p t a = (b, intCT solver t b p')
      where
        (b, p') = solver t0 y0 p1 t a

--intCT2 t0 y0 p1 = PCT {prCT = p}
--  where
--    p t a = (b, intCT2 t b p')
--      where
--        (b, p') = rk4 t0 y0 p1 t a


-- | Solvers collection.

-- | Trapezoidal rule
trapezoidal t0 y0 p1 t a = (b, p1'')
  where
    b = y0 + (t - t0)/2 * (fa + fb)
    (fa, p1') = prCT p1 t0 a
    (fb, p1'') = prCT p1' t a

-- | Runge-Kutta 4th order: reduces to Simpson's rule.
rk4 t0 y0 p1 t a = (b, lastP)
  where
    h = t - t0
    (k1, p1')   = prCT p1 t0 a
    (k2, p1'')  = prCT p1' (t0 + h/2) a
    (k3, lastP) = prCT p1'' t a
    b = y0 + h/6 * (k1 + 4 * k2 + k3)
