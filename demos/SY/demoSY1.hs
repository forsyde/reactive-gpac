module DemoSY1 where

import ForSyDe.Reactive

-- | 'p1' increments every input token.
p1 = mapSY (+1)
s1in = [0..9]
s1out = fst $ execSY p1 s1in


-- | 'p2' counts how many tokens have been processed.
p2 = mooreSY nsf dec 0
  where nsf = \s a -> s + 1
        dec = \s -> s + 1
s2in = [(),(),(),(),(),(),(),(),(),()]
s2out = fst $ execSY p2 s2in

-- | 'symmetricalSpring' is one of the ForSyDe-shallow-examples demo.
symmetricalSpring :: Double         -- ^ spring constant, @c@
                  -> Double         -- ^ mass, @m@
                  -> Double         -- ^ discretization timestep, T
                  -> Double         -- ^ initial displacemet
                  -> SM Double Double (Double, Double)
symmetricalSpring c m t x0 = mooreSY nsf out (x0,x0)
  where nsf (x1, x2) input = (out (x1,x2), x1)
        out (x1, x2) = (2 - t^2*c/m)*x1 - x2
sSpringIn = map (*0.1) [0..2000]
sSpringOut = fst $ execSY (symmetricalSpring 1 10 0.1 1) sSpringIn
