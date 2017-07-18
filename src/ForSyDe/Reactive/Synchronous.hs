{-# LANGUAGE FlexibleContexts #-}

module ForSyDe.Reactive.Synchronous where

--import Control.Monad.State.Lazy

data Process a b s = Moore (s -> a -> s) (s -> b) s




















-- data MapSY f = MapSY f

-- newtype Process a b s = Process {runProcess :: a -> (b, s)}

-- data Machine state event signal = Machine
--   { mCurState :: state,
--     mTransFunction :: state -> event -> (state, signal)}

-- -- newtype State (Machine state event signal) signal =
-- --   State {runState :: (Machine state event signal) -> (signal, (Machine state event signal))}

-- stepMachine :: event
--             -> Machine state event signal
--             -> (signal, Machine state event signal)
-- stepMachine event machine = (output, machine {mCurState = newState})
--   where curState = mCurState machine
--         (newState, output) = mTransFunction machine curState event

-- createMachine :: state
--               -> (state -> event -> (state, signal))
--               -> Machine state event signal
-- createMachine = Machine

-- iterateSSY :: Machine state event signal -> [event] -> [signal]
-- iterateSSY _ [] = []
-- iterateSSY m (x:xs) = y:(iterateSSY newM xs)
--   where (y, newM) = stepMachine x m

-- cascadeSSY :: Machine s0 a b
--            -> Machine s1 b c
--            -> a
--            -> c
-- cascadeSSY m1 m2 e = y
--   where s0 = fst $ stepMachine e m1
--         y = fst $ stepMachine s0 m2

-- -- (>>>) :: Machine state event signal
-- --       -> (state -> Machine state event signal)
-- --       -> Machine state event signal
-- -- m1 >>> f = let (a, newM1) = mTransFunction m1
-- --                g = f a
-- --            in g newM1

-- mapSSY f = createMachine 0 (\_ x -> (f x, f x))

-- delaySSY s0 = createMachine s0 (\s e -> (e, s))

-- mooreSSY nsf dec s0 = createMachine s0 (\s e -> (nsf s, dec s e))

-- mealySSY nsf dec s0 = createMachine s0 (\s e -> (nsf s e, dec s e))
















--data Process a b = Event a -> State s b

-- mapSY :: a -> State (a -> b) b
-- mapSY x = state $ \f -> (f x, f)

-- --delaySY :: a -> State a a
-- delaySY s0 = state $ \x -> (x, s0)


-- mooreSY :: (s -> a -> s)        -- ^ Next state function
--         -> (s -> b)             -- ^ Output decoder
--         -> a                    -- ^ Input token
--         -> State s b            -- ^ Output
-- mooreSY nsf dec x = state $ \s -> (dec s, nsf s x)

-- mealySY :: (s -> a -> s)        -- ^ Next state function
--         -> (s -> a -> b)        -- ^ Output decoder
--         -> a                    -- ^ Input token
--         -> State s b            -- ^ Output
-- mealySY nsf dec x = state $ \s -> (dec s x, nsf s x)

-- --p1 :: State Int Int
-- p1 x = mooreSY (\s x -> s+x) (id)

-- p2 = mapSY

-- --p3 = delaySY

-- --p4 = \x -> (delaySY x) >>= delaySY































-- data ProcessConstructorSY s a = MapSY (a -> s)
-- --                     | ZipWithSY (a -> b -> c)
--                      | DelaySY s
--                      | MooreSY (s -> a -> s) (s -> s) s
--                      | MealySY (s -> a -> s) (s -> a -> s) s

-- data Function = Function
-- data Value = Value
-- data State = State

-- data EventSY a = EventSY a deriving (Show)

-- evalP :: ProcessConstructorSY b a -> EventSY a -> EventSY b
-- evalP (MapSY f) (EventSY v) = EventSY $ f v
-- --evalP (ZipWithSY f) (EventSY (v1, v2)) = EventSY $ f v1 v2
-- evalP (DelaySY s) (EventSY v) = EventSY s
-- evalP (MooreSY nsf dec s) (EventSY v) = EventSY $ dec s
-- evalP (MealySY nsf dec s) (EventSY v) = EventSY $ dec s v

-- execP :: EventSY a -> ProcessConstructorSY a a -> (EventSY a, ProcessConstructorSY a a)
-- execP (EventSY v) (MapSY f) = (EventSY $ f v, MapSY f)
-- --execP (EventSY v) (ZipWithSY f) = (EventSY $ f v, ZipWithSY f)
-- execP (EventSY v) (DelaySY s) = (EventSY s, DelaySY v)
-- execP (EventSY v) (MooreSY nsf dec s)
--   = (EventSY $ dec s, MooreSY nsf dec (nsf s v))
-- execP (EventSY v) (MealySY nsf dec s)
--   = (EventSY $ dec s v, MealySY nsf dec (nsf s v))


-- return :: EventSY a -> ProcessConstructorSY s a -> (EventSY a, ProcessConstructorSY s a)
-- return e = \p -> (e, p)

--ss >>= g = \p -> let (a, newState) = execP ss

--data Process s a
--  = ProcessSY {runP :: (ProcessConstructorSY s a)
--                -> (EventSY a, ProcessConstructorSY s a)}



--data EventSY a = EventSY a
--type ProcessConstructorSY a b = EventSY a -> EventSY b
--type StateSY a b = (EventSY b, ProcessConstructorSY a b)

--mapSY :: (a -> b) -> EventSY a -> StateSY a b
--mapSY f (EventSY value) = (EventSY (f value), mapSY f)


--zipWithSY :: (a -> b -> c) -> Signal a -> Signal b -> Signal c

--delaySY :: a -> Signal a -> Signal a

--mooreSY :: (a -> b -> a) -- ^ Combinational function for next state decoder.
--        -> (a -> c)      -- ^ Combinational function for output decoder.
--        -> a             -- ^ Initial state
--        -> Signal b      -- ^ Input signal
--        -> Signal c      -- ^ Output signal

--mealySY :: (a -> b -> a) -- ^ Combinational function for next state decoder.
--        -> (a -> b -> c)      -- ^ Combinational function for output decoder.
--        -> a             -- ^ Initial state
--        -> Signal b      -- ^ Input signal
--        -> Signal c      -- ^ Output signal
