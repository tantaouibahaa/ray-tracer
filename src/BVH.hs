module BVH (mkBVHNode) where

import Data.List (sortBy)
import Vec3
import Ray
import Interval
import AABB
import Hittable
import Random

mkBVHNode :: [Hittable] -> RNG -> (Hittable, RNG)
mkBVHNode [] g = (Hittable (\_ _ -> Nothing) emptyAABB defaultPdfValue defaultHittableRandom, g)
mkBVHNode [obj] g = (obj, g)
mkBVHNode [a, b] g =
    let bbox = aabbFromBoxes (hittableBBox a) (hittableBBox b)
    in (Hittable (hitBVH a b bbox) bbox defaultPdfValue defaultHittableRandom, g)
mkBVHNode objects g =
    let allBBox = foldl (\acc o -> aabbFromBoxes acc (hittableBBox o)) emptyAABB objects
        axis = aabbLongestAxis allBBox
        sorted = sortBy (boxCompare axis) objects
        mid = length sorted `div` 2
        (leftList, rightList) = splitAt mid sorted
        (left, g1) = mkBVHNode leftList g
        (right, g2) = mkBVHNode rightList g1
        bbox = aabbFromBoxes (hittableBBox left) (hittableBBox right)
    in (Hittable (hitBVH left right bbox) bbox defaultPdfValue defaultHittableRandom, g2)

hitBVH :: Hittable -> Hittable -> AABB -> Ray -> Interval -> Maybe HitRecord
hitBVH left right bbox ray interval =
    if not (aabbHit bbox ray interval)
    then Nothing
    else
        let hitLeft = hittableHit left ray interval
            rightInterval = case hitLeft of
                Nothing  -> interval
                Just rec -> Interval (intervalMin interval) (hitT rec)
            hitRight = hittableHit right ray rightInterval
        in case hitRight of
            Just _  -> hitRight
            Nothing -> hitLeft

boxCompare :: Int -> Hittable -> Hittable -> Ordering
boxCompare axis a b =
    let ai = aabbAxisInterval (hittableBBox a) axis
        bi = aabbAxisInterval (hittableBBox b) axis
    in compare (intervalMin ai) (intervalMin bi)
