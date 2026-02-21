module ONB where

import Vec3

data ONB = ONB
    { onbU :: !Vec3
    , onbV :: !Vec3
    , onbW :: !Vec3
    }

mkONB :: Vec3 -> ONB
mkONB n =
    let w = unitVector n
        a = if abs (vecX w) > 0.9 then Vec3 0 1 0 else Vec3 1 0 0
        v = unitVector (cross w a)
        u = cross w v
    in ONB u v w

onbTransform :: ONB -> Vec3 -> Vec3
onbTransform (ONB u v w) (Vec3 x y z) =
    vecScale x u `vecAdd` vecScale y v `vecAdd` vecScale z w
