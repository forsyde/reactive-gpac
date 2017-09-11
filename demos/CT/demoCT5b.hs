module DemoCT5b where

import ForSyDe.Reactive

-- | Sigma delta modulator

-- | System input
sineSource = (intCT rk4 0 0 p1 &&& constCT (0.1)) >>> multCT
  where p1 = (constCT (-1) *** idCT) >>> multCT >>> openLoopInt
        openLoopInt = intCT rk4 0 1 p2
        p2 = (idCT *** constCT 0) >>> adderCT


-- | Comparator and inverter
compInv = switchCT (\x -> x < 0) (constCT 1) (constCT (-1))

-- | Comparator
comp = switchCT (\x -> x < 0) (constCT (-1)) (constCT 1)

-- | GPAC "polynomial" circuit. Feedback will be achieved over the
-- compInv process input.
polyCirc = (sineSource *** compInv) >>> adderCT

-- | Feedback system.
modulator = intCT rk4 0 0 polyCirc

-- | Simulation setup

-- | Sampling points
tt = linspace 0 (2*pi) 512

-- | Observing the system
ss = fst $ execCT (time >>> modulator >>> comp) tt
ssine = fst $ execCT (time >>> sineSource) tt

-- | Dumping the information for plot
main = do
  writeDump "demos/plot/sdm_out.dat" ss
  writeDump "demos/plot/sine.dat" ssine
