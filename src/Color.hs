module Color where

import System.IO
import Vec3
import Interval

linearToGamma :: Double -> Double
linearToGamma x = if x > 0 then sqrt x else 0

writeColor :: Handle -> Vec3 -> IO ()
writeColor handle pixelColor =
    let r0 = vecX pixelColor
        g0 = vecY pixelColor
        b0 = vecZ pixelColor
        r1 = if isNaN r0 then 0 else r0
        g1 = if isNaN g0 then 0 else g0
        b1 = if isNaN b0 then 0 else b0
        intensity = Interval 0.000 0.999
        r = clamp intensity (linearToGamma r1)
        g = clamp intensity (linearToGamma g1)
        b = clamp intensity (linearToGamma b1)
        ir = floor (256 * r) :: Int
        ig = floor (256 * g) :: Int
        ib = floor (256 * b) :: Int
    in hPutStrLn handle (show ir ++ " " ++ show ig ++ " " ++ show ib)
