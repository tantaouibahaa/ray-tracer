module Sphere (mkSphere, mkMovingSphere) where

import Vec3
import Ray
import Interval
import AABB
import Hittable
import ONB
import Random

mkSphere :: Point3 -> Double -> Material -> Hittable
mkSphere center radius mat = Hittable hitFn bbox pdfValueFn randomFn
  where
    rvec = Vec3 radius radius radius
    bbox = aabbFromPoints (center `vecSub` rvec) (center `vecAdd` rvec)
    hitFn = hitStaticSphere center (max 0 radius) mat

    pdfValueFn origin direction =
        case hitFn (Ray origin direction 0) (Interval 0.001 (1/0)) of
            Nothing -> 0
            Just _ ->
                let distSquared = vecLengthSquared (center `vecSub` origin)
                    cosThetaMax = sqrt (1 - radius * radius / distSquared)
                    solidAngle = 2 * pi * (1 - cosThetaMax)
                in 1 / solidAngle

    randomFn origin g =
        let direction = center `vecSub` origin
            distSquared = vecLengthSquared direction
            uvw = mkONB direction
            (localDir, g') = randomToSphere radius distSquared g
        in (onbTransform uvw localDir, g')

mkMovingSphere :: Point3 -> Point3 -> Double -> Material -> Hittable
mkMovingSphere center1 center2 radius mat = Hittable hitFn bbox defaultPdfValue defaultHittableRandom
  where
    rvec = Vec3 radius radius radius
    box1 = aabbFromPoints (center1 `vecSub` rvec) (center1 `vecAdd` rvec)
    box2 = aabbFromPoints (center2 `vecSub` rvec) (center2 `vecAdd` rvec)
    bbox = aabbFromBoxes box1 box2
    centerVec = center2 `vecSub` center1
    sphereCenter t = center1 `vecAdd` vecScale t centerVec
    hitFn ray interval =
        let center = sphereCenter (rayTime ray)
        in hitStaticSphere center (max 0 radius) mat ray interval

hitStaticSphere :: Point3 -> Double -> Material -> Ray -> Interval -> Maybe HitRecord
hitStaticSphere center radius mat ray interval =
    let oc = center `vecSub` rayOrigin ray
        a = vecLengthSquared (rayDirection ray)
        h = dot (rayDirection ray) oc
        c = vecLengthSquared oc - radius * radius
        discriminant = h * h - a * c
    in if discriminant < 0
       then Nothing
       else
         let sqrtd = sqrt discriminant
             root1 = (h - sqrtd) / a
             root2 = (h + sqrtd) / a
             tryRoot root =
                if not (surrounds interval root)
                   then Nothing
                   else
                     let p = rayAt ray root
                         outwardNormal = vecDiv (p `vecSub` center) radius
                         (ff, n) = setFaceNormal ray outwardNormal
                         (u, v) = getSphereUV outwardNormal
                     in Just HitRecord
                         { hitPoint = p
                         , hitNormal = n
                         , hitT = root
                         , hitU = u
                         , hitV = v
                         , hitFrontFace = ff
                         , hitMaterial = mat
                         }
         in case tryRoot root1 of
              Just record -> Just record
              Nothing     -> tryRoot root2

getSphereUV :: Point3 -> (Double, Double)
getSphereUV p =
    let theta = acos (-(vecY p))
        phi = atan2 (-(vecZ p)) (vecX p) + pi
        u = phi / (2 * pi)
        v = theta / pi
    in (u, v)

randomToSphere :: Double -> Double -> RNG -> (Vec3, RNG)
randomToSphere radius distSquared g0 =
    let (r1, g1) = randomDouble g0
        (r2, g2) = randomDouble g1
        z = 1 + r2 * (sqrt (1 - radius * radius / distSquared) - 1)
        phi = 2 * pi * r1
        sqrtPart = sqrt (1 - z * z)
        x = cos phi * sqrtPart
        y = sin phi * sqrtPart
    in (Vec3 x y z, g2)
