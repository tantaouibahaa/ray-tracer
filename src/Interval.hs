module Interval where

data Interval = Interval
    { intervalMin :: !Double
    , intervalMax :: !Double
    }

emptyInterval :: Interval
emptyInterval = Interval (1/0) (-(1/0))

universeInterval :: Interval
universeInterval = Interval (-(1/0)) (1/0)

intervalSize :: Interval -> Double
intervalSize (Interval lo hi) = hi - lo

intervalExpand :: Double -> Interval -> Interval
intervalExpand delta (Interval lo hi) =
    let padding = delta / 2
    in Interval (lo - padding) (hi + padding)

intervalUnion :: Interval -> Interval -> Interval
intervalUnion (Interval lo1 hi1) (Interval lo2 hi2) =
    Interval (min lo1 lo2) (max hi1 hi2)

intervalShift :: Double -> Interval -> Interval
intervalShift offset (Interval lo hi) = Interval (lo + offset) (hi + offset)

surrounds :: Interval -> Double -> Bool
surrounds (Interval lo hi) x = lo < x && x < hi

contains :: Interval -> Double -> Bool
contains (Interval lo hi) x = lo <= x && x <= hi

clamp :: Interval -> Double -> Double
clamp (Interval lo hi) x
    | x < lo    = lo
    | x > hi    = hi
    | otherwise  = x
