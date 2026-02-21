module Perlin where

import Data.Array (Array, listArray, (!), (//))
import Data.Bits (xor)
import Vec3
import Random

pointCount :: Int
pointCount = 256

data Perlin = Perlin
    { perlinRandvec :: !(Array Int Vec3)
    , perlinPermX   :: !(Array Int Int)
    , perlinPermY   :: !(Array Int Int)
    , perlinPermZ   :: !(Array Int Int)
    }

mkPerlin :: RNG -> (Perlin, RNG)
mkPerlin g0 =
    let (vecs, g1) = generateVecs pointCount g0 []
        randvec = listArray (0, pointCount - 1) vecs
        (px, g2) = perlinGeneratePerm g1
        (py, g3) = perlinGeneratePerm g2
        (pz, g4) = perlinGeneratePerm g3
    in (Perlin randvec px py pz, g4)

generateVecs :: Int -> RNG -> [Vec3] -> ([Vec3], RNG)
generateVecs 0 g acc = (reverse acc, g)
generateVecs n g acc =
    let (v, g') = randomVec3Range (-1) 1 g
    in generateVecs (n - 1) g' (unitVector v : acc)

perlinGeneratePerm :: RNG -> (Array Int Int, RNG)
perlinGeneratePerm g0 =
    let initial = listArray (0, pointCount - 1) [0 .. pointCount - 1]
        (shuffled, g1) = permute (pointCount - 1) initial g0
    in (shuffled, g1)

permute :: Int -> Array Int Int -> RNG -> (Array Int Int, RNG)
permute 0 arr g = (arr, g)
permute i arr g =
    let (target, g') = randomInt 0 i g
        vi = arr ! i
        vt = arr ! target
        arr' = arr // [(i, vt), (target, vi)]
    in permute (i - 1) arr' g'

perlinNoise :: Perlin -> Point3 -> Double
perlinNoise p (Vec3 px py pz) =
    let u = px - fromIntegral (floor px :: Int)
        v = py - fromIntegral (floor py :: Int)
        w = pz - fromIntegral (floor pz :: Int)
        i = floor px :: Int
        j = floor py :: Int
        k = floor pz :: Int
        c = [ [ [ perlinRandvec p ! ((perlinPermX p ! ((i + di) `mod` pointCount))
                    `xor` (perlinPermY p ! ((j + dj) `mod` pointCount))
                    `xor` (perlinPermZ p ! ((k + dk) `mod` pointCount)))
                | dk <- [0, 1] ]
              | dj <- [0, 1] ]
            | di <- [0, 1] ]
    in perlinInterp c u v w

perlinInterp :: [[[Vec3]]] -> Double -> Double -> Double -> Double
perlinInterp c u v w =
    let uu = u * u * (3 - 2 * u)
        vv = v * v * (3 - 2 * v)
        ww = w * w * (3 - 2 * w)
    in sum [ let i' = fromIntegral di
                 j' = fromIntegral dj
                 k' = fromIntegral dk
                 weightV = Vec3 (u - i') (v - j') (w - k')
             in (i' * uu + (1 - i') * (1 - uu))
              * (j' * vv + (1 - j') * (1 - vv))
              * (k' * ww + (1 - k') * (1 - ww))
              * dot ((c !! di !! dj) !! dk) weightV
           | di <- [0, 1], dj <- [0, 1], dk <- [0, 1] ]

perlinTurb :: Perlin -> Point3 -> Int -> Double
perlinTurb p point depth =
    let go 0 _ _ acc = abs acc
        go d pt w acc =
            let n = perlinNoise p pt
            in go (d - 1) (vecScale 2 pt) (w * 0.5) (acc + w * n)
    in go depth point 1.0 0.0
