module Main where

import System.IO
import Color
import Vec3
import Ray


writePpmHeader :: Handle -> Int -> Int -> IO ()
writePpmHeader handle width height = do
    hPutStrLn handle "P3"
    hPutStrLn handle (show width ++ " " ++ show height)
    hPutStrLn handle "255"


rayColor :: Ray -> Vec3
rayColor ray =
    let unitDirection = unitVector (rayDirection ray)
        a = 0.5 * (vecY unitDirection + 1.0)
        white = Vec3 1.0 1.0 1.0
        blue  = Vec3 0.5 0.7 1.0
    in vecAdd (vecScale (1.0 - a) white) (vecScale a blue)

main :: IO ()
main = do
    let aspectRatio = 16.0 / 9.0 :: Double
        imageWidth  = 400 :: Int
        imageHeight = max 1 (floor (fromIntegral imageWidth / aspectRatio)) :: Int

        focalLength    = 1.0
        viewportHeight = 2.0
        viewportWidth  = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)
        cameraCenter   = Vec3 0 0 0

        viewportU = Vec3 viewportWidth 0 0
        viewportV = Vec3 0 (-viewportHeight) 0

        pixelDeltaU = vecDiv viewportU (fromIntegral imageWidth)
        pixelDeltaV = vecDiv viewportV (fromIntegral imageHeight)

        viewportUpperLeft = cameraCenter
            `vecSub` Vec3 0 0 focalLength
            `vecSub` vecDiv viewportU 2
            `vecSub` vecDiv viewportV 2
        pixel00Loc = viewportUpperLeft
            `vecAdd` vecScale 0.5 (pixelDeltaU `vecAdd` pixelDeltaV)

    fileHandle <- openFile "image.ppm" WriteMode
    writePpmHeader fileHandle imageWidth imageHeight

    mapM_ (\row -> do
        hPutStr stderr ("\rScanlines remaining: " ++ show (imageHeight - row) ++ " ")
        hFlush stderr
        mapM_ (\col ->
            let pixelCenter = pixel00Loc
                    `vecAdd` vecScale (fromIntegral col) pixelDeltaU
                    `vecAdd` vecScale (fromIntegral row) pixelDeltaV
                rayDir = pixelCenter `vecSub` cameraCenter
                ray = Ray cameraCenter rayDir
                pixelColor = rayColor ray
            in writeColor fileHandle pixelColor
            ) [0 .. imageWidth - 1]
        ) [0 .. imageHeight - 1]

    hPutStrLn stderr "\rDone.                    "
    hClose fileHandle
