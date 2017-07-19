module ForSyDe.Reactive.CoreSY where

import ForSyDe.Reactive.CoreTypeSY

-- | Process constructors: we define the usual ForSyDe process
-- constructors. These are immutable processes in the sense once they are created, their functionality does not change.

-- | 'mealySY' builds a mealy machine process.
mealySY :: (s -> a -> s)        -- ^ Next state decoder.
        -> (s -> a -> b)        -- ^ Output decoder.
        -> s                    -- ^ Initial state.
        -> SM a b s             -- ^ Mealy state machine process.
mealySY nsf dec s0 = SM (st) s0
  where st = SF (\s a -> (dec s a, SM (st) (nsf s a)))

-- | 'mooreSY' builds a moore machine process.
mooreSY :: (s -> a -> s)        -- ^ Next state decoder.
        -> (s -> b)             -- ^ Output decoder.
        -> s                    -- ^ Initial state.
        -> SM a b s             -- ^ Moore state machine process.
mooreSY nsf dec s0 = SM (st) s0
  where st = SF (\s a -> (dec s, SM (st) (nsf s a)))

-- | 'delaySY' delays the input by one event cycle by introducing an
-- initial value at the beginning of the output signal.
delaySY :: s                    -- ^ Initial state.
        -> SM s s s             -- ^ Delay process.
delaySY s0 = SM (sf) s0
  where sf = SF (\s a -> (s, SM (sf) a))

-- | 'mapSY' builds a combinational process.
mapSY :: (a -> b)               -- ^ Combinational function.
      -> SM a b ()              -- ^ Combinational process.
mapSY f = SM (sf) ()
  where sf = SF (\s a -> (f a, SM (sf) ()))


-- | Mutable (reconfigurable) processes?



-- | Processes execution.

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
