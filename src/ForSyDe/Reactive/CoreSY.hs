module ForSyDe.Reactive.CoreSY where

import ForSyDe.Reactive.CoreTypeSY

-- | 'stepSY' executes a process for one cycle.
stepSY :: SM a b s -> a -> (b, SM a b s)
stepSY (SM (SF sf) s) = sf s

-- | 'execSY' executes a process over an input stream.
execSY :: SM a b s
       -> [a]
       -> ([b], SM a b s)
execSY sm [] = ([], sm)
execSY (SM (SF sf) s) (x:xs) = (b:bs, finalP)
  where (b, newP) = stepSY (SM (SF sf) s) x
        (bs, finalP) = execSY newP xs
