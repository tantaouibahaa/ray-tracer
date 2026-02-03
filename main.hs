import System.IO


main :: IO ()
main = do

    let width = 256
    let height = 256

    h <- openFile "image.ppm" WriteMode
    hPutStrLn h "P3"
    hPutStrLn h (show width ++ " " ++ show height)
    hPutStrLn h "255"


    sequence_ [ 
          let r = fromIntegral i / fromIntegral (width -1)
              g = fromIntegral (height -1 - j) / fromIntegral (height - 1)
              b = 0.25 :: Double
              ir = floor (255.999 * r) :: Int
              ig = floor (255.999 * g) :: Int
              ib = floor (255.999 * b) :: Int
          in putStrLn (show ir ++ " " ++ show ig ++ " " ++ show ib)
            | i <- [0..width-1]
            , j <- [0..height-1]
        ]
    
    hPutStr stderr "Done. "
    putStrLn "Hello, world!"

