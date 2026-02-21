module Texture where

import Data.Word (Word8)
import qualified Data.Vector.Storable as VS
import qualified Codec.Picture as JP
import Vec3
import Perlin

data ImageData = ImageData
    { imgWidth  :: !Int
    , imgHeight :: !Int
    , imgPixels :: !(VS.Vector Word8)
    }

data Texture
    = SolidColor !Vec3
    | CheckerTexture !Double Texture Texture
    | ImageTexture ImageData
    | NoiseTexture Perlin !Double

textureValue :: Texture -> Double -> Double -> Point3 -> Vec3
textureValue (SolidColor c) _ _ _ = c
textureValue (CheckerTexture invScale t1 t2) u v p =
    let x = floor (invScale * vecX p) :: Int
        y = floor (invScale * vecY p) :: Int
        z = floor (invScale * vecZ p) :: Int
        isEven = even (x + y + z)
    in if isEven
       then textureValue t1 u v p
       else textureValue t2 u v p
textureValue (ImageTexture img) u v _ =
    let u' = clampVal 0 1 u
        v' = 1.0 - clampVal 0 1 v
        i = min (floor (u' * fromIntegral (imgWidth img))) (imgWidth img - 1)
        j = min (floor (v' * fromIntegral (imgHeight img))) (imgHeight img - 1)
        colorScale = 1.0 / 255.0
        idx = (j * imgWidth img + i) * 3
        pixels = imgPixels img
        r = colorScale * fromIntegral (pixels VS.! idx)
        g = colorScale * fromIntegral (pixels VS.! (idx + 1))
        b = colorScale * fromIntegral (pixels VS.! (idx + 2))
    in Vec3 r g b
textureValue (NoiseTexture perl scale) _ _ p =
    vecScale (0.5 * (1.0 + sin (scale * vecZ p + 10 * perlinTurb perl p 7))) (Vec3 1 1 1)

clampVal :: Double -> Double -> Double -> Double
clampVal lo hi x
    | x < lo    = lo
    | x > hi    = hi
    | otherwise = x

loadImage :: FilePath -> IO (Maybe ImageData)
loadImage path = do
    result <- JP.readImage path
    case result of
        Left _ -> return Nothing
        Right dynImg ->
            let img = JP.convertRGB8 dynImg
                w = JP.imageWidth img
                h = JP.imageHeight img
                pixels = JP.imageData img
            in return (Just (ImageData w h pixels))
