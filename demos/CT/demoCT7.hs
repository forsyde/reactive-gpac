module DemoCT7 where

import ForSyDe.Reactive

-- | Experimenting with different implementations of the RK4

-- | Reference
sineCirc = intCT rk4 0 0 p1
  where p1 = (constCT (-1) *** idCT) >>> multCT >>> openLoopInt
        openLoopInt = intCT rk4 0 1 p2
        p2 = (idCT *** constCT 0) >>> adderCT

-- | Reference Process
s1 = time >>> sineCirc


-- | Modified: seems that the output loop "controls" the error... For
-- the RK4, the sin vanishes while with trap the system becomes
-- unstable.
sineCirc' = intCT trap 0 0 p1
  where p1 = (constCT (-1) *** idCT) >>> multCT >>> openLoopInt
        openLoopInt = intCT rk4 0 1 p2
        p2 = (idCT *** constCT 0) >>> adderCT

-- | Alternative Process
s2 = time >>> sineCirc'


-- | For the open loop integrals, rk4 and trap look pretty much the
-- same.
openLoopIntRK = intCT rk4 0 0 p2
  where p2 = (idCT *** constCT 0) >>> adderCT
openLoopIntTrap = intCT trap 0 0 p2
  where p2 = (idCT *** constCT 0) >>> adderCT

s3 = time >>> (liftCT sin)
s4 = time >>> (liftCT cos) >>> openLoopIntRK
s5 = time >>> (liftCT cos) >>> openLoopIntTrap


-- | Observation points
tt = linspace 0 (10*2*pi) 1000

-- | Execute processes
ss1 = fst $ execCT s1 tt
ss2 = fst $ execCT s2 tt
ss3 = fst $ execCT s3 tt
ss4 = fst $ execCT s4 tt
ss5 = fst $ execCT s5 tt


-- | Dump results
main = do
  writeDump "demos/plot/ref_sin.dat" ss1
  writeDump "demos/plot/mod_sin.dat" ss2
  writeDump "demos/plot/sin.dat" ss3
  writeDump "demos/plot/cos_rk.dat" ss4
  writeDump "demos/plot/cos_trap.dat" ss5
