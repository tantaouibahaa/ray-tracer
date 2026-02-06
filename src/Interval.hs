module Interval where

data Interval = Interval
    { intervalMin :: !Double
    , intervalMax :: !Double
    }

surrounds :: Interval -> Double -> Bool
surrounds (Interval lo hi) x = lo < x && x < hi

contains :: Interval -> Double -> Bool
contains (Interval lo hi) x = lo <= x && x <= hi

clamp :: Interval -> Double -> Double
clamp (Interval lo hi) x
    | x < lo    = lo
    | x > hi    = hi
    | otherwise  = x
