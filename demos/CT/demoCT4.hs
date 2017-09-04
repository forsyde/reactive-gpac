module DemoCT4 where

import ForSyDe.Reactive

-- | Open loop integrator
p1 = (liftCT id *** constCT 0) >>> adderCT
pp = time >>> liftCT cos >>> intCT rk4 0 0 p1

pp' = time >>> liftCT sin

tt = linspace 0 (10*2*pi) 10000
ss = fst $ execCT pp tt
ss' = fst $ execCT pp' tt

-- | Sin circuit
openLoopInt = intCT rk4 0 1 p1
polyCirc = (constCT (-1) *** liftCT id) >>> multCT >>> openLoopInt
sineCirc = time >>> intCT rk4 0 0 polyCirc

sine = fst $ execCT sineCirc tt

main = do
  writeDump "demos/plot/int_cos.dat" ss
  writeDump "demos/plot/sin.dat" ss'
  writeDump "demos/plot/sine_circ.dat" sine
