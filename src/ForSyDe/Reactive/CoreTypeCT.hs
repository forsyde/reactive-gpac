{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}

module ForSyDe.Reactive.CoreTypeCT where

-- | We represent time as doubles even tough this is not the best
-- representation. For now we are interested in establishing the
-- framework.
type Time = Double

-- | The outputs of our processes will be doubles as well, but it will
-- be better to change latter to the same representation used for
-- Time.
-- newtype Value = Double

-- | Signals are functions over Time.
--data SignalCT a = SignalCT (Time -> (a, SignalCT a))

-- | Processes are functions over signals.
data PCT a b = PCT {prCT :: Time -> a -> ContCT a b}

-- | The Continuation data type encapsulates the output of the process
-- and a new Process for later computations.
type ContCT a b = (b, PCT a b)

-- | Sources have no input.
type SourceCT a = PCT () a

-- | API:

-- | 'at' observes a PCT at a specified time t.
at :: PCT () a -> Time -> a
p `at` t = fst $ prCT p t ()

-- | 'nextCT' skips one time step
nextCT :: PCT () b -> Time -> PCT () b
nextCT p t = snd $ prCT p t ()

-- | 'signalCT' extracts a signal from a process.
-- | IMPORTANT: you lose the continuation features and might mess with
-- the dynamic behavior of integrators.
signalCT :: PCT a b -> (Time -> a -> b)
signalCT (PCT {prCT = p1}) = \t a -> fst $ p1 t a

-- | Lifting functions to processes
liftCT :: (a -> b) -> PCT a b
liftCT f = PCT {prCT = \_ a -> (f a, liftCT f)}

mergeCT :: (a -> b -> c) -> PCT (a,b) c
mergeCT f = liftCT $ uncurry f


-- | Composition operators.
cascadeCT :: PCT a b
          -> PCT b c
          -> PCT a c
cascadeCT p1 p2 = PCT {prCT = p}
  where
    p t a = (c, p1' `cascadeCT` p2')
      where
        (b, p1') = prCT p1 t a
        (c, p2') = prCT p2 t b

(>>>) :: PCT a b
      -> PCT b c
      -> PCT a c
(>>>) = cascadeCT


(<<<) :: PCT b c
       -> PCT a b
       -> PCT a c
p1 <<< p2 = cascadeCT p2 p1

-- | Parallel composition: splitCT p1 id
firstCT :: PCT a b
        -> PCT (a, c) (b, c)
firstCT p1 = PCT {prCT = p}
  where
    p t (a,c) = ((b,c), firstCT p')
      where
        (b, p') = prCT p1 t a

-- | Parallel composition: splitCT id p1
secondCT :: PCT a b
         -> PCT (c, a) (c, b)
secondCT p1 = PCT {prCT = p}
  where
    p t (c,a) = ((c,b), secondCT p')
      where
        (b, p') = prCT p1 t a

-- | Parallel composition
parallelCT :: PCT a b
        -> PCT c d
        -> PCT (a,c) (b,d)
parallelCT p1 p2 = PCT {prCT = p}
  where
    p t (a,c) = ((b,d), splitCT p1' p2')
      where
        (b, p1') = prCT p1 t a
        (d, p2') = prCT p2 t c

(***) :: PCT a b
      -> PCT c d
      -> PCT (a,c) (b,d)
(***) = parallelCT

-- | Split single input into two
feedCT :: PCT a b
       -> PCT a (b,b)
--feedCT p1 = PCT {prCT = p}
--  where
--    p t a = ((b,b), feedCT p1')
--      where
--        (b, p1') = prCT p1 t a
feedCT p1 = p1 >>> (idCT &&& idCT)
  where idCT = liftCT id


-- | Feeds single input to two processes
fanoutCT :: PCT a b
         -> PCT a c
         -> PCT a (b,c)
fanoutCT p1 p2 = PCT {prCT = p}
  where
    p t a = ((b,c), fanoutCT p1' p2')
      where
        (b, p1') = prCT p1 t a
        (c, p2') = prCT p2 t a

(&&&) :: PCT a b
      -> PCT a c
      -> PCT a (b,c)
(&&&) = fanoutCT

-- | Feedback operator
feedbackCT :: PCT (a,c) (b,c)
           -> PCT a b
feedbackCT p1 = PCT {prCT = p}
  where
    p t a = (b, feedbackCT p1')
      where
        ((b,c), p1') = prCT p1 t (a, c)
