module Vec3 where

data Vec3 = Vec3 !Double !Double !Double
    deriving (Show, Eq)

type Point3 = Vec3
type Color = Vec3

vecX (Vec3 x _ _) = x
vecY (Vec3 _ y _) = y
vecZ (Vec3 _ _ z) = z


vecAdd :: Vec3 -> Vec3 -> Vec3
vecAdd (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

vecSub :: Vec3 -> Vec3 -> Vec3
vecSub (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

vecMul :: Vec3 -> Vec3 -> Vec3
vecMul (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)

vecScale :: Double -> Vec3 -> Vec3
vecScale t (Vec3 x y z) = Vec3 (t * x) (t * y) (t * z)

vecDiv :: Vec3 -> Double -> Vec3
vecDiv v t = vecScale (1/t) v

vecNegate :: Vec3 -> Vec3
vecNegate (Vec3 x y z) = Vec3 (-x) (-y) (-z)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    Vec3 (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)

vecLength :: Vec3 -> Double
vecLength v = sqrt (vecLengthSquared v)

vecLengthSquared :: Vec3 -> Double
vecLengthSquared (Vec3 x y z) = x*x + y*y + z*z

unitVector :: Vec3 -> Vec3
unitVector v = vecDiv v (vecLength v)
