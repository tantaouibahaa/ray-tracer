module Sphere where

import Vec3
import Hittable
import Vec3

data Sphere = Sphere
   {
     sphereCenter :: !Point3
    ,sphereRadius :: !Double
   }

hitSphere :: Sphere -> Ray -> Double -> Double -> Maybe HitRecord
hitSphere sphere ray tMin tMax = 
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
                if root <= tMin || tMax <= root
                   then Nothing
                   else
                     let p = rayAt ray root
                         normal = vecDev (p `vecSub` center) radius
                     in Just (HitRecord p normal root)
         in case tryRoot root1 of
              Just record -> Just record
              Nothing -> tryRoot root2
