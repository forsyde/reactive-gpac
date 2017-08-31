module ForSyDe.Reactive.Utils where

import ForSyDe.Reactive.CoreTypeCT


-- -- | Processes execution: they output streams of DE events (Time, Value).
-- -- | stepCT
stepCT :: PCT () b -> Time -> ((Time,b), PCT () b)
stepCT p1 t = ((t, p1 `at` t), nextCT p1 t)

-- -- | execCT
execCT :: PCT () b -> [Time] -> ([(Time,b)], PCT () b)
execCT p1 [] = ([], p1)
execCT p1 (t:ts) = (b:bs, finalP)
  where (b, newP) = stepCT p1 t
        (bs, finalP) = execCT newP ts


-- | Writes a csv filse suitable to Gnuplot
writeDump :: (Show a) => String -> [(Time, a)] -> IO()
writeDump filename points = writeFile filename (dumpSig points)

-- | Auxiliary function to create the output strings
dumpSig :: (Show a) => [(Time,a)] -> String
dumpSig points = concatMap f points
  where f x = show (fst x) ++ "\t" ++ show (snd x) ++ "\n"


-- | Creates an uniform grid list including interval extrema.
linspace :: Double -> Double -> Int -> [Double]
linspace start stop n = map (scale) nodes
  where nodes = map (fromIntegral) [0..n-1]
        scale x = x * length / (fromIntegral $ n-1) + start
        length = stop - start

-- | Creates a Chebyshev grid list
chebyspace :: Double -> Double -> Int -> [Double]
chebyspace start stop n = map (scale) nodes
  where --n_2 = fromIntegral $ n * 2
        --nodes = map (getNode . fromIntegral) $ reverse [1..n]
        --getNode x = cos $ pi/n_2 * (2*x-1)
    nodes = reverse $ map cos $ linspace 0 pi n
    scale x = 1/2*(start + stop) + 1/2*(stop - start) * x
