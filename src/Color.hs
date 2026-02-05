module Color where

import System.IO
import Vec3

writeColor :: Handle -> Vec3 -> IO ()
writeColor handle pixelColor =
    let scaledRed   = floor (255.999 * vecX pixelColor) :: Int
        scaledGreen = floor (255.999 * vecY pixelColor) :: Int
        scaledBlue  = floor (255.999 * vecZ pixelColor) :: Int
    in hPutStrLn handle (show scaledRed ++ " " ++ show scaledGreen ++ " " ++ show scaledBlue)
