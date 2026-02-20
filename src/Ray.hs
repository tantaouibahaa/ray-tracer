module Ray where
import Vec3

data Ray = Ray
    { rayOrigin    :: !Point3
    , rayDirection :: !Vec3
    , rayTime      :: !Double
    }

mkRay :: Point3 -> Vec3 -> Ray
mkRay o d = Ray o d 0

mkRayWithTime :: Point3 -> Vec3 -> Double -> Ray
mkRayWithTime = Ray

rayAt :: Ray -> Double -> Point3
rayAt ray t = rayOrigin ray `vecAdd` vecScale t (rayDirection ray)
