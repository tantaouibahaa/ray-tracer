module Ray where
import Vec3

data Ray = Ray {
    rayOrigin :: !Point3,
    rayDirection:: !Vec3
}

rayAt :: Ray -> Double -> Point3
rayAt ray t = rayOrigin ray `vecAdd` vecScale t (rayDirection ray)
