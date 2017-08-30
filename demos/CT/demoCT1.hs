module DemoCT1 where

import ForSyDe.Reactive

-- | Constant output:
-- | p1(t) = 1
p1 = constCT 1

-- | Integral of p1
-- | Expected to be p2(t) = t
p2 = intCT 0 0 p1

-- | Integral of p2
-- | Expected to be p3(t) = t^2
p3 = intCT 0 0 p2

-- | Sine function
-- | p4(t) = sin(t)
-- | Note that we have to feed it with time...
p4 = time >>>* liftCT sin

-- | Time vector to sample the signals, these are `n` observation
-- times including start and stop.
obsTime n start stop = t
  where
    t = map (\x -> (x/(n-1))*(stop - start) + start) [0..n-1]

s1 = fst $ execCT p1 $ obsTime 11 0 10
s2 = fst $ execCT p2 $ obsTime 11 0 10
s3 = fst $ execCT p3 $ obsTime 11 0 10
s4 = fst $ execCT p4 $ obsTime 11 0 (2*pi)
