module DemoCT3 where

import ForSyDe.Reactive

-- | Exponential function
p1 = time >>> liftCT exp

-- | Feedback system that implements the exponential
p2 = time >>> (intCT rk4' 0 1 $ liftCT id)

-- | Sampling points
tt = linspace 0 1 21

-- | Simulation
ss1 = fst $ execCT p1 tt
ss2 = fst $ execCT p2 tt

-- | Dumping for plot
main = do
  writeDump "demos/plot/exp.dat" ss1
  writeDump "demos/plot/exp_feedback.dat" ss2
