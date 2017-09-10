module DemoCT6 where

import ForSyDe.Reactive

-- | Found the bug: constCT (-1) on p1 definition ignores the time input. There is an implicit time input which is accessible via the `at` function and so far I can not alter time from that...

-- | Sine circuit
sineCirc = intCT rk4 0 0 p1
  where p1 = (constCT (-1) *** idCT) >>> multCT >>> openLoopInt
        openLoopInt = intCT rk4 0 1 p2
        p2 = (idCT *** constCT 0) >>> adderCT

sineCirc'' = liftCT sin

-- | Reference Process
s1 = time >>> sineCirc

-- | Time shifted process
s2 = time >>> tShift (pi/2) >>> sineCirc
  where tShift k = (idCT &&& constCT k) >>> adderCT

s2' = time >>> tShift (pi/2)
  where tShift k = (idCT &&& constCT k) >>> adderCT


-- | Time scaled process
s3 = time >>> tScale 2 >>> sineCirc
  where tScale k = (idCT &&& constCT k) >>> multCT

s3' = time >>> tScale 2
  where tScale k = (idCT &&& constCT k) >>> multCT

-- | Observation points
tt = linspace 0 (2*pi) 100

-- | Execute processes
ss1 = fst $ execCT s1 tt
ss2 = fst $ execCT s2 tt
ss3 = fst $ execCT s3 tt

-- | Dump results
main = do
  writeDump "demos/plot/ref_sin.dat" ss1
  writeDump "demos/plot/sft_sin.dat" ss2
  writeDump "demos/plot/scl_sin.dat" ss3
