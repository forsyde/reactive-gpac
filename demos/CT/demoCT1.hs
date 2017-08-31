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
p4 = time >>>* liftCT sin

p5 = intCT rk4 0 (-1) p4


-- | Uniform sampling
t1 = linspace 0 10 10

s1 = fst $ execCT p1 t1
s2 = fst $ execCT p2 t1
s3 = fst $ execCT p3 t1


-- | Uniform sampling
t2 = linspace 0 (5*2*pi) 100
s4 = fst $ execCT p4 t2
s5 = fst $ execCT p5 t2

-- | Chebyshev (nonuniform) sampling
t3 = chebyspace 0 (5*2*pi) 100
s4' = fst $ execCT p4 t3
s5' = fst $ execCT p5 t3

main = do
  writeDump "demos/plot/unif_sin.dat" s4
  writeDump "demos/plot/unif_int_sin.dat" s5
  writeDump "demos/plot/cheby_sin.dat" s4'
  writeDump "demos/plot/cheby_int_sin.dat" s5'
