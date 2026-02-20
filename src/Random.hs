module Random where

import System.Random (StdGen, uniformR)
import Vec3

type RNG = StdGen

randomInt :: Int -> Int -> RNG -> (Int, RNG)
randomInt lo hi = uniformR (lo, hi)

randomDouble :: RNG -> (Double, RNG)
randomDouble = uniformR (0.0, 1.0)

randomDoubleRange :: Double -> Double -> RNG -> (Double, RNG)
randomDoubleRange lo hi = uniformR (lo, hi)

randomVec3 :: RNG -> (Vec3, RNG)
randomVec3 g0 =
    let (x, g1) = randomDouble g0
        (y, g2) = randomDouble g1
        (z, g3) = randomDouble g2
    in (Vec3 x y z, g3)

randomVec3Range :: Double -> Double -> RNG -> (Vec3, RNG)
randomVec3Range lo hi g0 =
    let (x, g1) = randomDoubleRange lo hi g0
        (y, g2) = randomDoubleRange lo hi g1
        (z, g3) = randomDoubleRange lo hi g2
    in (Vec3 x y z, g3)

randomInUnitSphere :: RNG -> (Vec3, RNG)
randomInUnitSphere g =
    let (v, g') = randomVec3Range (-1) 1 g
    in if vecLengthSquared v < 1
       then (v, g')
       else randomInUnitSphere g'

randomUnitVector :: RNG -> (Vec3, RNG)
randomUnitVector g =
    let (v, g') = randomInUnitSphere g
    in (unitVector v, g')

randomInUnitDisk :: RNG -> (Vec3, RNG)
randomInUnitDisk g =
    let (x, g1) = randomDoubleRange (-1) 1 g
        (y, g2) = randomDoubleRange (-1) 1 g1
        v = Vec3 x y 0
    in if vecLengthSquared v < 1
       then (v, g2)
       else randomInUnitDisk g2

randomCosineDirection :: RNG -> (Vec3, RNG)
randomCosineDirection g0 =
    let (r1, g1) = randomDouble g0
        (r2, g2) = randomDouble g1
        phi = 2 * pi * r1
        x = cos phi * sqrt r2
        y = sin phi * sqrt r2
        z = sqrt (1 - r2)
    in (Vec3 x y z, g2)
