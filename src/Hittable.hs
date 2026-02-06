module Hittable where

import Ray
import Vec3

data HitRecord = HitRecord 
    { hitPoint :: !Point3
    , hitNormal :: !Vec3
    , hitT      :: !Double
    }
