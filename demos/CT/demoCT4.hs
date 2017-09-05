module DemoCT4 where

import ForSyDe.Reactive

-- | Benchmark: the feedback system is supposed to generate a sin
-- function
pp = time >>> liftCT sin

-- | Open loop integrator
openLoopInt = intCT rk4 0 1 poly1
  -- Open loop integrator polynomial circuit. It takes the feedback
  -- signal and changes it into a zero which is them summed with the
  -- input.
  where poly1 = (liftCT id *** constCT 0) >>> adderCT

-- | Sin circuit
polyCirc = (constCT (-1) *** liftCT id) >>> multCT >>> openLoopInt
sineCirc = time >>> intCT rk4 0 0 polyCirc

-- | Looking at the simulation, will you note that the sine generated
-- by the feedback circuit vanishes over time. I believe this is a
-- limitation with the fixed step ODE solver that is implemented
-- (RK4). That behavior improves by having closer observation points.
-- Here is the comparison between a sin and int cos.
cosInt = time >>> (liftCT cos) >>> openLoopInt00
  where openLoopInt00 = intCT rk4 0 0 p
        p = (liftCT id *** constCT 0) >>> adderCT


-- | Observation points
tt = linspace 0 (10*2*pi) 1000

-- | Simulation
sine = fst $ execCT pp tt
sineFeedback = fst $ execCT sineCirc tt
cosineInt = fst $ execCT cosInt tt


-- | Dumping for plot
main = do
  writeDump "demos/plot/sin.dat" sine
  writeDump "demos/plot/sine_circ.dat" sineFeedback
  writeDump "demos/plot/cosInt.dat" cosineInt
