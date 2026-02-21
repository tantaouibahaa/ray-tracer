module Quad (mkQuad, mkBox) where

import Vec3
import Ray
import Interval
import AABB
import Hittable
import Random

mkQuad :: Point3 -> Vec3 -> Vec3 -> Material -> Hittable
mkQuad q u v mat = Hittable hitFn bbox pdfValueFn randomFn
  where
    n = cross u v
    normal = unitVector n
    d = dot normal q
    w = vecDiv n (dot n n)
    area = vecLength n
    bbox = padToMin $ aabbFromPoints q (q `vecAdd` u `vecAdd` v)
    padToMin b = aabbFromBoxes b (AABB (intervalExpand 0.0001 (aabbX b))
                                       (intervalExpand 0.0001 (aabbY b))
                                       (intervalExpand 0.0001 (aabbZ b)))

    hitFn ray interval =
        let denom = dot normal (rayDirection ray)
        in if abs denom < 1e-8
           then Nothing
           else
             let t = (d - dot normal (rayOrigin ray)) / denom
             in if not (surrounds interval t)
                then Nothing
                else
                  let intersection = rayAt ray t
                      planarHitPt = intersection `vecSub` q
                      alpha = dot w (cross planarHitPt v)
                      beta  = dot w (cross u planarHitPt)
                  in if not (isInterior alpha beta)
                     then Nothing
                     else
                       let (ff, fn) = setFaceNormal ray normal
                       in Just HitRecord
                           { hitPoint = intersection
                           , hitNormal = fn
                           , hitT = t
                           , hitU = alpha
                           , hitV = beta
                           , hitFrontFace = ff
                           , hitMaterial = mat
                           }

    isInterior a b = a >= 0 && a <= 1 && b >= 0 && b <= 1

    pdfValueFn origin direction =
        case hitFn (Ray origin direction 0) (Interval 0.001 (1/0)) of
            Nothing -> 0
            Just rec ->
                let distSquared = hitT rec * hitT rec * vecLengthSquared direction
                    cosine = abs (dot direction (hitNormal rec) / vecLength direction)
                in if cosine < 0.000001 then 0
                   else distSquared / (cosine * area)

    randomFn origin g =
        let (r1, g1) = randomDouble g
            (r2, g2) = randomDouble g1
            p = q `vecAdd` vecScale r1 u `vecAdd` vecScale r2 v
        in (p `vecSub` origin, g2)

mkBox :: Point3 -> Point3 -> Material -> Hittable
mkBox (Vec3 x0 y0 z0) (Vec3 x1 y1 z1) mat =
    let mn = Vec3 (min x0 x1) (min y0 y1) (min z0 z1)
        mx = Vec3 (max x0 x1) (max y0 y1) (max z0 z1)
        dx = Vec3 (vecX mx - vecX mn) 0 0
        dy = Vec3 0 (vecY mx - vecY mn) 0
        dz = Vec3 0 0 (vecZ mx - vecZ mn)
    in mkHittableList
        [ mkQuad (Vec3 (vecX mn) (vecY mn) (vecZ mx)) dx dy mat
        , mkQuad (Vec3 (vecX mx) (vecY mn) (vecZ mx)) (vecNegate dz) dy mat
        , mkQuad (Vec3 (vecX mx) (vecY mn) (vecZ mn)) (vecNegate dx) dy mat
        , mkQuad (Vec3 (vecX mn) (vecY mn) (vecZ mn)) dz dy mat
        , mkQuad (Vec3 (vecX mn) (vecY mx) (vecZ mx)) dx (vecNegate dz) mat
        , mkQuad (Vec3 (vecX mn) (vecY mn) (vecZ mn)) dx dz mat
        ]
