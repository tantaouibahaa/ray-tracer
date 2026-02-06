module Hittable where

import Vec3
import Ray
import Interval

data Material
    = Lambertian !Vec3
    | Metal !Vec3 !Double
    | Dielectric !Double

data HitRecord = HitRecord
    { hitPoint    :: !Point3
    , hitNormal   :: !Vec3
    , hitT        :: !Double
    , hitFrontFace :: !Bool
    , hitMaterial :: !Material
    }

setFaceNormal :: Ray -> Vec3 -> (Bool, Vec3)
setFaceNormal ray outwardNormal =
    let frontFace = dot (rayDirection ray) outwardNormal < 0
        normal = if frontFace then outwardNormal else vecNegate outwardNormal
    in (frontFace, normal)

hitWorld :: [a] -> (a -> Ray -> Interval -> Maybe HitRecord) -> Ray -> Interval -> Maybe HitRecord
hitWorld objects hitFn ray interval = foldr go Nothing objects
  where
    go obj acc =
        let tMax = maybe (intervalMax interval) hitT acc
            int  = Interval (intervalMin interval) tMax
        in case hitFn obj ray int of
            Just rec -> Just rec
            Nothing  -> acc
