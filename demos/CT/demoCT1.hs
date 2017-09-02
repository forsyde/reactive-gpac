module DemoCT1 where

import ForSyDe.Reactive

-- | Constant output:
-- | p1(t) = 1
p1 = constCT 1

-- | Integral of p1
-- | Expected to be p2(t) = t
p2 = intCT rk4 0 0 p1

-- | Integral of p2
-- | Expected to be p3(t) = t^2
p3 = intCT rk4 0 0 p2

-- | Sine function
-- | p4(t) = sin(t)
-- | Note that we have to feed it with time...
p4 = time >>> liftCT sin

p5 = intCT rk4 0 (-1) p4


-- | Uniform sampling
t1 = linspace 0 10 10

s1 = fst $ execCT p1 t1
s2 = fst $ execCT p2 t1
s3 = fst $ execCT p3 t1


-- | Nonuniform sampling
t2 = chebyspace 0 (5*2*pi) 50
s4 = fst $ execCT p4 t2
s5 = fst $ execCT p5 t2

-- | Chebyshev (nonuniform) sampling. This has common points compared
-- to t4 and the system is expected to yield the same results on
-- those.
t3 = chebyspace 0 (5*2*pi) 99
s4' = fst $ execCT p4 t3
s5' = fst $ execCT p5 t3

-- | "Efficiency" verification: put a big number on n
n = 1000
t4 = linspace 0 (5*2*pi) n
s4'' = fst $ execCT p4 t4
s5'' = fst $ execCT p5 t4


-- | Testing "discrete" changes...
pp1 = time >>> liftCT change
  where change t
          | t <= (t2 !! 25) = 1.0
          | otherwise = (-1.0)

pp2 = liftCT (uncurry (-))

pp3 = feedCT (liftCT id) >>> firstCT pp1 >>> pp2

tt = linspace 0 10 100
ss = fst $ execCT (intCT rk4' 0 0 pp3) tt

ss1 = fst $ execCT (intCT rk4' 0 0 pp3) t2
ss2 = fst $ execCT (intCT rk4' 0 0 pp3) t3


main = do
  writeDump "demos/plot/cheby_sin1.dat" s4
  writeDump "demos/plot/cheby_int_sin1.dat" s5
  writeDump "demos/plot/cheby_sin2.dat" s4'
  writeDump "demos/plot/cheby_int_sin2.dat" s5'

  writeDump "demos/plot/eff_sin.dat" s4''
  writeDump "demos/plot/eff_int_sin.dat" s5''

  writeDump "demos/plot/rc_filter.dat" ss
  writeDump "demos/plot/cheby_rc1.dat" ss1
  writeDump "demos/plot/cheby_rc2.dat" ss2
