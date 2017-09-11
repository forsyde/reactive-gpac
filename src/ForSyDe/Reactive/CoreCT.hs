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
--adderCT :: (Num b) => PCT a (b, b) -> PCT a b
--adderCT (PCT {prCT = p1}) = PCT {prCT = p}
--  where
--    p t a = (uncurry (+) c, adderCT p1')
--      where
--        (c, p1') = p1 t a
adderCT :: (Num a) => PCT (a,a) a
--adderCT = liftCT (uncurry (+))
adderCT = mergeCT (+)

-- | Multiplier
--multCT :: (Num b) => PCT a (b, b) -> PCT a b
--multCT (PCT {prCT = p1}) = PCT {prCT = p}
--  where
--    p t a = (uncurry (*) c, adderCT p1')
--      where
--        (c, p1') = p1 t a
multCT :: (Num a) => PCT (a, a) a
--multCT = liftCT (uncurry (*))
multCT = mergeCT (*)

-- | Identity process.
idCT :: PCT a a
idCT = liftCT id

-- | Switch between two processes with stateless predicate
switchCT :: (a -> Bool)
       -> PCT a b
       -> PCT a b
       -> PCT a b
switchCT predicate p1 p2 = PCT p
  where
    p t a
      | predicate a == True = (fst $ prCT p1 t a, switchCT predicate (snd $ prCT p1 t a) p2)
      | otherwise           = (fst $ prCT p2 t a, switchCT predicate p1 (snd $ prCT p2 t a))



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

-- | Runge-Kutta 4th order: reduces to Simpson's rule.
rk4 t0 y0 p1 t a = (b, lastP)
  where
    h = t - t0
    (k1, p1_1) = prCT p1 t0 (a, y0)
    (k2, p1_2) = prCT p1_1 (t0 + h/2) (a, y0 + h/2*k1)
    (k3, p1_3) = prCT p1_2 (t0 + h/2) (a, y0 + h/2*k2)
    (k4, lastP) = prCT p1_3 (t0 + h) (a, y0 + h*k3)
    b = y0 + h/6*(k1 + 2*k2 + 2*k3 + k4)

-- | Trying to improve RK4 method, but yields the same results as the
-- one above.
rk4teste t0 y0 p1 t a = (b, lastP)
  where
    h = t - t0
    k1 = (time >>> (constCT a &&& constCT y0) >>>  p1) `at` t0
    (_,p11) = prCT p1 (t0) (a, y0)
    k2 = (time >>> (constCT a &&& constCT (y0 + h/2*k1)) >>>  p11) `at` (t0 + h/2)
    (_,p12) = prCT p11 (t0 + h/2) (a, y0 + h/2*k1)
    k3 = (time >>> (constCT a &&& constCT (y0 + h/2*k2)) >>>  p12) `at` (t0 + h/2)
    (_,p13) = prCT p12 (t0 + h/2) (a, y0 + h/2*k2)
    k4 = (time >>> (constCT a &&& constCT (y0 + h*k3)) >>>  p13) `at` (t0 + h)
    b = y0 + h/6*(k1 + 2*k2 + 2*k3 + k4)
    (_,lastP) = prCT p13 t (a, y0 + h*k3)

-- | Single input. Not intended to be used.
rk4' t0 y0 p1 t a = (b, lastP)
  where
    h = t - t0
    (k1, p1_1) = prCT p1 t0 y0
    (k2, p1_2) = prCT p1_1 (t0 + h/2) (y0 + h/2*k1)
    (k3, p1_3) = prCT p1_2 (t0 + h/2) (y0 + h/2*k2)
    (k4, lastP) = prCT p1_3 (t0 + h) (y0 + h*k3)
    b = y0 + h/6*(k1 + 2*k2 + 2*k3 + k4)

-- | Trapezoidal method.
trap t0 y0 p1 t a = (b, p1'')
  where
    b = y0 + (t - t0)/2 * (fa + fb)
    (fa, p1') = prCT p1 t0 (a, y0)
    (fb, p1'') = prCT p1' t (a, y0)

-- | Trapezoidal rule, single input. Not intended to be used.
trapezoidal t0 y0 p1 t a = (b, p1'')
  where
    b = y0 + (t - t0)/2 * (fa + fb)
    (fa, p1') = prCT p1 t0 a
    (fb, p1'') = prCT p1' t a
