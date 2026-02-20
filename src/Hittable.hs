module Hittable where

import Vec3
import Ray
import Interval
import AABB
import Texture
import Random

data Material
    = Lambertian Texture
    | Metal !Vec3 !Double
    | Dielectric !Double
    | DiffuseLight Texture
    | Isotropic Texture

data HitRecord = HitRecord
    { hitPoint     :: !Point3
    , hitNormal    :: !Vec3
    , hitT         :: !Double
    , hitU         :: !Double
    , hitV         :: !Double
    , hitFrontFace :: !Bool
    , hitMaterial  :: Material
    }

data PDF = PDF
    { pdfValue    :: Vec3 -> Double
    , pdfGenerate :: RNG -> (Vec3, RNG)
    }

data ScatterRecord
    = DiffuseScatter !Vec3 PDF
    | SpecularScatter !Vec3 !Ray

data Hittable = Hittable
    { hittableHit       :: Ray -> Interval -> Maybe HitRecord
    , hittableBBox      :: AABB
    , hittablePdfValue  :: Point3 -> Vec3 -> Double
    , hittableRandom    :: Point3 -> RNG -> (Vec3, RNG)
    }

setFaceNormal :: Ray -> Vec3 -> (Bool, Vec3)
setFaceNormal ray outwardNormal =
    let frontFace = dot (rayDirection ray) outwardNormal < 0
        normal = if frontFace then outwardNormal else vecNegate outwardNormal
    in (frontFace, normal)

defaultPdfValue :: Point3 -> Vec3 -> Double
defaultPdfValue _ _ = 0

defaultHittableRandom :: Point3 -> RNG -> (Vec3, RNG)
defaultHittableRandom _ g = (Vec3 1 0 0, g)

mkHittableList :: [Hittable] -> Hittable
mkHittableList objects = Hittable hitFn bbox pdfValueFn randomFn
  where
    bbox = foldl aabbFromBoxes emptyAABB (map hittableBBox objects)
    hitFn ray interval = foldl go Nothing objects
      where
        go acc obj =
            let tMax = maybe (intervalMax interval) hitT acc
                int  = Interval (intervalMin interval) tMax
            in case hittableHit obj ray int of
                Just rec -> Just rec
                Nothing  -> acc
    pdfValueFn origin direction =
        let n = length objects
        in if n == 0 then 0
           else let weight = 1.0 / fromIntegral n
                in sum [weight * hittablePdfValue obj origin direction | obj <- objects]
    randomFn origin g =
        let n = length objects
        in if n == 0 then (Vec3 1 0 0, g)
           else let (idx, g') = randomInt 0 (n - 1) g
                in hittableRandom (objects !! idx) origin g'

mkTranslate :: Hittable -> Vec3 -> Hittable
mkTranslate object offset = Hittable hitFn bbox defaultPdfValue defaultHittableRandom
  where
    bbox = aabbShift (hittableBBox object) offset
    hitFn ray interval =
        let offsetRay = Ray (rayOrigin ray `vecSub` offset) (rayDirection ray) (rayTime ray)
        in case hittableHit object offsetRay interval of
            Nothing  -> Nothing
            Just rec -> Just rec
                { hitPoint = hitPoint rec `vecAdd` offset
                }

mkRotateY :: Hittable -> Double -> Hittable
mkRotateY object angle = Hittable hitFn bbox defaultPdfValue defaultHittableRandom
  where
    radians = angle * pi / 180.0
    sinTheta = sin radians
    cosTheta = cos radians
    origBBox = hittableBBox object

    bbox =
        let xs = aabbX origBBox
            ys = aabbY origBBox
            zs = aabbZ origBBox
            corners = [ Vec3 x y z
                      | x <- [intervalMin xs, intervalMax xs]
                      , y <- [intervalMin ys, intervalMax ys]
                      , z <- [intervalMin zs, intervalMax zs]
                      ]
            rotated = map rotatePoint corners
            rotatePoint (Vec3 x y z) = Vec3 (cosTheta * x + sinTheta * z) y (-sinTheta * x + cosTheta * z)
            inf = 1/0
            minV = foldl (\(Vec3 ax ay az) (Vec3 bx by bz) -> Vec3 (min ax bx) (min ay by) (min az bz)) (Vec3 inf inf inf) rotated
            maxV = foldl (\(Vec3 ax ay az) (Vec3 bx by bz) -> Vec3 (max ax bx) (max ay by) (max az bz)) (Vec3 (-inf) (-inf) (-inf)) rotated
        in aabbFromPoints minV maxV

    hitFn ray interval =
        let o = rayOrigin ray
            d = rayDirection ray
            origin = Vec3 (cosTheta * vecX o - sinTheta * vecZ o) (vecY o) (sinTheta * vecX o + cosTheta * vecZ o)
            direction = Vec3 (cosTheta * vecX d - sinTheta * vecZ d) (vecY d) (sinTheta * vecX d + cosTheta * vecZ d)
            rotatedRay = Ray origin direction (rayTime ray)
        in case hittableHit object rotatedRay interval of
            Nothing  -> Nothing
            Just rec ->
                let p = hitPoint rec
                    n = hitNormal rec
                    newPoint = Vec3 (cosTheta * vecX p + sinTheta * vecZ p) (vecY p) (-sinTheta * vecX p + cosTheta * vecZ p)
                    newNormal = Vec3 (cosTheta * vecX n + sinTheta * vecZ n) (vecY n) (-sinTheta * vecX n + cosTheta * vecZ n)
                    (ff, fn) = setFaceNormal ray newNormal
                in Just rec { hitPoint = newPoint, hitNormal = fn, hitFrontFace = ff }

mkConstantMedium :: Hittable -> Double -> Material -> RNG -> (Hittable, RNG)
mkConstantMedium boundary density phaseMat g0 = (Hittable hitFn (hittableBBox boundary) defaultPdfValue defaultHittableRandom, g0)
  where
    negInvDensity = -1.0 / density
    hitFn ray (Interval tmin tmax) =
        case hittableHit boundary ray (Interval (-(1/0)) (1/0)) of
            Nothing -> Nothing
            Just rec1 ->
                case hittableHit boundary ray (Interval (hitT rec1 + 0.0001) (1/0)) of
                    Nothing -> Nothing
                    Just rec2 ->
                        let t1 = max (hitT rec1) tmin
                            t2 = min (hitT rec2) tmax
                        in if t1 >= t2 then Nothing
                           else let t1' = max t1 0
                                    rayLen = vecLength (rayDirection ray)
                                    distInsideBoundary = (t2 - t1') * rayLen
                                    hitDist = negInvDensity * log (pseudoRandom (rayTime ray) t1')
                                in if hitDist > distInsideBoundary then Nothing
                                   else let t = t1' + hitDist / rayLen
                                        in Just HitRecord
                                            { hitPoint = rayAt ray t
                                            , hitNormal = Vec3 1 0 0
                                            , hitT = t
                                            , hitU = 0
                                            , hitV = 0
                                            , hitFrontFace = True
                                            , hitMaterial = phaseMat
                                            }

    pseudoRandom a b =
        let x = abs (sin (a * 12.9898 + b * 78.233) * 43758.5453)
        in x - fromIntegral (floor x :: Int)
