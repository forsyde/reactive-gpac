module DemoCT5 where

import ForSyDe.Reactive

-- | System input
sineSource = liftCT (\t -> 0.1*(sin t))

-- | Comparator and inverter
compInv = liftCT f
  where f x
          | x < 0 = 1
          | otherwise = -1

-- | Comparator
comp = liftCT f
  where f x
          | x < 0 = -1
          |otherwise = 1

-- | GPAC "polynomial" circuit. Feedback will be achieved over the
-- compInv process input.
polyCirc = (sineSource *** compInv) >>> adderCT

-- | Feedback system.
modulator = intCT rk4 0 0 polyCirc

-- | Simulation setup

-- | Sampling points
tt = linspace 0 (2*2*pi) 2048

-- | Observing the system
ss = fst $ execCT (time >>> modulator >>> comp) tt
ssine = fst $ execCT (time >>> sineSource) tt

-- | Dumping the information for plot
main = do
  writeDump "demos/plot/sdm_out.dat" ss
  writeDump "demos/plot/sine.dat" ssine
