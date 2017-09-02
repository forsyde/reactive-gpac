module DemoCT2 where

import ForSyDe.Reactive

-- | Sine function: p1(t) = sin(t)
p1 = time >>> liftCT sin

-- | Time shifting: p2(t) = sin(2*t)
p2 = time >>> tScale 2 >>> liftCT sin
  where tScale k = (liftCT id &&& constCT k) >>> multCT

-- | Time shifting: p3(t) = sin(t + pi/2)
p3 = time >>> tShift (pi/2) >>> liftCT sin
  where tShift k = (liftCT id &&& constCT k) >>> adderCT

-- | Sample times
tt = linspace 0 (2*pi) 50

-- | Execute models
ss1 = fst $ execCT p1 tt
ss2 = fst $ execCT p2 tt
ss3 = fst $ execCT p3 tt

-- | Dump outputs
main = do
  writeDump "demos/plot/sin_t.dat" ss1
  writeDump "demos/plot/sin_2t.dat" ss2
  writeDump "demos/plot/sin_t_pi2.dat" ss3
