module AABB where

import Vec3
import Ray
import Interval

data AABB = AABB
    { aabbX :: !Interval
    , aabbY :: !Interval
    , aabbZ :: !Interval
    }

emptyAABB :: AABB
emptyAABB = AABB emptyInterval emptyInterval emptyInterval

universeAABB :: AABB
universeAABB = AABB universeInterval universeInterval universeInterval

aabbFromPoints :: Point3 -> Point3 -> AABB
aabbFromPoints (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    padToMinimums $ AABB
        (Interval (min x1 x2) (max x1 x2))
        (Interval (min y1 y2) (max y1 y2))
        (Interval (min z1 z2) (max z1 z2))

aabbFromBoxes :: AABB -> AABB -> AABB
aabbFromBoxes a b = AABB
    (intervalUnion (aabbX a) (aabbX b))
    (intervalUnion (aabbY a) (aabbY b))
    (intervalUnion (aabbZ a) (aabbZ b))

aabbAxisInterval :: AABB -> Int -> Interval
aabbAxisInterval box 0 = aabbX box
aabbAxisInterval box 1 = aabbY box
aabbAxisInterval box _ = aabbZ box

aabbLongestAxis :: AABB -> Int
aabbLongestAxis box
    | sx > sy && sx > sz = 0
    | sy > sz            = 1
    | otherwise          = 2
  where
    sx = intervalSize (aabbX box)
    sy = intervalSize (aabbY box)
    sz = intervalSize (aabbZ box)

aabbHit :: AABB -> Ray -> Interval -> Bool
aabbHit box ray (Interval tmin0 tmax0) =
    go 0 tmin0 tmax0
  where
    go 3 _ _ = True
    go axis tmin tmax =
        let ax = aabbAxisInterval box axis
            adinv = 1.0 / vecIndex (rayDirection ray) axis
            orig = vecIndex (rayOrigin ray) axis
            t0 = (intervalMin ax - orig) * adinv
            t1 = (intervalMax ax - orig) * adinv
            (tlo, thi) = if t0 < t1 then (t0, t1) else (t1, t0)
            tmin' = max tlo tmin
            tmax' = min thi tmax
        in if tmax' <= tmin' then False else go (axis + 1) tmin' tmax'

aabbShift :: AABB -> Vec3 -> AABB
aabbShift (AABB ix iy iz) (Vec3 ox oy oz) = AABB
    (intervalShift ox ix)
    (intervalShift oy iy)
    (intervalShift oz iz)

padToMinimums :: AABB -> AABB
padToMinimums (AABB ix iy iz) = AABB (pad ix) (pad iy) (pad iz)
  where
    delta = 0.0001
    pad i = if intervalSize i < delta then intervalExpand delta i else i
