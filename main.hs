import System.IO

main :: IO ()
main = do
    let imageWidth = 256
    let imageHeight = 256
    fileHandle <- openFile "image.ppm" WriteMode
    hPutStrLn fileHandle "P3"
    hPutStrLn fileHandle (show imageWidth ++ " " ++ show imageHeight)
    hPutStrLn fileHandle "255"

    mapM_ (\row -> do
        hPutStr stderr ("\rScanlines remaining: " ++ show (imageHeight - row) ++ " ")
        hFlush stderr
        mapM_ (\col ->
            let red   = fromIntegral col / fromIntegral (imageWidth - 1)
                green = fromIntegral (imageHeight - 1 - row) / fromIntegral (imageHeight - 1)
                blue  = 0.25 :: Double
                scaledRed   = floor (255.999 * red)   :: Int
                scaledGreen = floor (255.999 * green) :: Int
                scaledBlue  = floor (255.999 * blue)  :: Int
            in hPutStrLn fileHandle (show scaledRed ++ " " ++ show scaledGreen ++ " " ++ show scaledBlue)
            ) [0 .. imageWidth - 1]
        ) [0 .. imageHeight - 1]

    hPutStrLn stderr "\rDone.                    "
    hClose fileHandle
