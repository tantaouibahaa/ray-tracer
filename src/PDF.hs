module PDF where

import Vec3
import Random
import ONB
import Hittable

mkCosinePdf :: Vec3 -> PDF
mkCosinePdf normal = PDF valueFn genFn
  where
    uvw = mkONB normal
    valueFn dir =
        let cosTheta = dot (unitVector dir) (onbW uvw)
        in max 0 (cosTheta / pi)
    genFn g =
        let (d, g') = randomCosineDirection g
        in (onbTransform uvw d, g')

mkSpherePdf :: PDF
mkSpherePdf = PDF valueFn genFn
  where
    valueFn _ = 1 / (4 * pi)
    genFn = randomUnitVector

mkHittablePdf :: Hittable -> Point3 -> PDF
mkHittablePdf obj origin = PDF valueFn genFn
  where
    valueFn dir = hittablePdfValue obj origin dir
    genFn g = hittableRandom obj origin g

mkMixturePdf :: PDF -> PDF -> PDF
mkMixturePdf p0 p1 = PDF valueFn genFn
  where
    valueFn dir = 0.5 * pdfValue p0 dir + 0.5 * pdfValue p1 dir
    genFn g =
        let (r, g') = randomDouble g
        in if r < 0.5
           then pdfGenerate p0 g'
           else pdfGenerate p1 g'
