module Sphere where

import Vec3
import Ray
import Interval
import Hittable

data Sphere = Sphere
    { sphereCenter   :: !Point3
    , sphereRadius   :: !Double
    , sphereMaterial :: !Material
    }

hitSphere :: Sphere -> Ray -> Interval -> Maybe HitRecord
hitSphere sphere ray interval =
    let center = sphereCenter sphere
        radius = max 0 (sphereRadius sphere)
        oc = center `vecSub` rayOrigin ray
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
                     in Just (HitRecord p n root ff (sphereMaterial sphere))
         in case tryRoot root1 of
              Just record -> Just record
              Nothing     -> tryRoot root2
