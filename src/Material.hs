module Material where

import Vec3
import Ray
import Hittable
import Random

scatter :: Material -> Ray -> HitRecord -> RNG -> Maybe (Vec3, Ray, RNG)
scatter (Lambertian albedo) _ rec g =
    let (rv, g') = randomUnitVector g
        scatterDir0 = hitNormal rec `vecAdd` rv
        scatterDir = if nearZero scatterDir0 then hitNormal rec else scatterDir0
        scattered = Ray (hitPoint rec) scatterDir
    in Just (albedo, scattered, g')
scatter (Metal albedo fuzz) rayIn rec g =
    let reflected = reflect (rayDirection rayIn) (hitNormal rec)
        (rv, g') = randomUnitVector g
        fuzzed = unitVector reflected `vecAdd` vecScale (min fuzz 1.0) rv
        scattered = Ray (hitPoint rec) fuzzed
    in if dot fuzzed (hitNormal rec) > 0
       then Just (albedo, scattered, g')
       else Nothing
scatter (Dielectric refIdx) rayIn rec g =
    let attenuation = Vec3 1 1 1
        ri = if hitFrontFace rec then 1.0 / refIdx else refIdx
        unitDir = unitVector (rayDirection rayIn)
        cosTheta = min (dot (vecNegate unitDir) (hitNormal rec)) 1.0
        sinTheta = sqrt (1.0 - cosTheta * cosTheta)
        cannotRefract = ri * sinTheta > 1.0
        (randVal, g') = randomDouble g
        shouldReflect = cannotRefract || reflectance cosTheta ri > randVal
        direction = if shouldReflect
                    then reflect unitDir (hitNormal rec)
                    else refract unitDir (hitNormal rec) ri
        scattered = Ray (hitPoint rec) direction
    in Just (attenuation, scattered, g')

reflectance :: Double -> Double -> Double
reflectance cosine refIdx =
    let r0 = ((1 - refIdx) / (1 + refIdx)) ^ (2 :: Int)
    in r0 + (1 - r0) * (1 - cosine) ^ (5 :: Int)
