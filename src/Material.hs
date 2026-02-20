module Material where

import Vec3
import Ray
import Hittable
import Texture
import Random
import PDF

scatter :: Material -> Ray -> HitRecord -> RNG -> Maybe (ScatterRecord, RNG)
scatter (Lambertian tex) _ rec g =
    let attenuation = textureValue tex (hitU rec) (hitV rec) (hitPoint rec)
    in Just (DiffuseScatter attenuation (mkCosinePdf (hitNormal rec)), g)
scatter (Metal albedo fuzz) rayIn rec g =
    let reflected = reflect (rayDirection rayIn) (hitNormal rec)
        (rv, g') = randomUnitVector g
        fuzzed = unitVector reflected `vecAdd` vecScale (min fuzz 1.0) rv
        specRay = Ray (hitPoint rec) fuzzed (rayTime rayIn)
    in Just (SpecularScatter albedo specRay, g')
scatter (Dielectric refIdx) rayIn rec g =
    let ri = if hitFrontFace rec then 1.0 / refIdx else refIdx
        unitDir = unitVector (rayDirection rayIn)
        cosTheta = min (dot (vecNegate unitDir) (hitNormal rec)) 1.0
        sinTheta = sqrt (1.0 - cosTheta * cosTheta)
        cannotRefract = ri * sinTheta > 1.0
        (randVal, g') = randomDouble g
        shouldReflect = cannotRefract || reflectance cosTheta ri > randVal
        direction = if shouldReflect
                    then reflect unitDir (hitNormal rec)
                    else refract unitDir (hitNormal rec) ri
        specRay = Ray (hitPoint rec) direction (rayTime rayIn)
    in Just (SpecularScatter (Vec3 1 1 1) specRay, g')
scatter (DiffuseLight _) _ _ _ = Nothing
scatter (Isotropic tex) _ rec g =
    let attenuation = textureValue tex (hitU rec) (hitV rec) (hitPoint rec)
    in Just (DiffuseScatter attenuation mkSpherePdf, g)

scatteringPdf :: Material -> Ray -> HitRecord -> Ray -> Double
scatteringPdf (Lambertian _) _ rec scattered =
    let cosTheta = dot (hitNormal rec) (unitVector (rayDirection scattered))
    in max 0 (cosTheta / pi)
scatteringPdf (Isotropic _) _ _ _ = 1 / (4 * pi)
scatteringPdf _ _ _ _ = 0

emitted :: Material -> Ray -> HitRecord -> Double -> Double -> Point3 -> Vec3
emitted (DiffuseLight tex) _ rec u v p =
    if hitFrontFace rec
    then textureValue tex u v p
    else Vec3 0 0 0
emitted _ _ _ _ _ _ = Vec3 0 0 0

reflectance :: Double -> Double -> Double
reflectance cosine refIdx =
    let r0 = ((1 - refIdx) / (1 + refIdx)) ^ (2 :: Int)
    in r0 + (1 - r0) * (1 - cosine) ^ (5 :: Int)
