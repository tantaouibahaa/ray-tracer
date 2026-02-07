module Color where

import System.IO
import Vec3
import Interval

linearToGamma :: Double -> Double
linearToGamma x = if x > 0 then sqrt x else 0

writeColor :: Handle -> Vec3 -> IO ()
writeColor handle pixelColor =
    let intensity = Interval 0.000 0.999
        r = clamp intensity (linearToGamma (vecX pixelColor))
        g = clamp intensity (linearToGamma (vecY pixelColor))
        b = clamp intensity (linearToGamma (vecZ pixelColor))
        ir = floor (256 * r) :: Int
        ig = floor (256 * g) :: Int
        ib = floor (256 * b) :: Int
    in hPutStrLn handle (show ir ++ " " ++ show ig ++ " " ++ show ib)
