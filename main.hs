module Main where

import System.IO
import System.Random (mkStdGen)
import Vec3
import Hittable
import Sphere
import Camera
import Random

finalScene :: RNG -> ([Sphere], RNG)
finalScene g0 =
    let groundMat = Lambertian (Vec3 0.5 0.5 0.5)
        ground = Sphere (Vec3 0 (-1000) 0) 1000 groundMat

        big1 = Sphere (Vec3 0 1 0) 1.0 (Dielectric 1.5)
        big2 = Sphere (Vec3 (-4) 1 0) 1.0 (Lambertian (Vec3 0.4 0.2 0.1))
        big3 = Sphere (Vec3 4 1 0) 1.0 (Metal (Vec3 0.7 0.6 0.5) 0.0)

        (smallSpheres, gFinal) = generateSmall (-11) (-11) g0 []

    in (ground : big1 : big2 : big3 : smallSpheres, gFinal)

generateSmall :: Int -> Int -> RNG -> [Sphere] -> ([Sphere], RNG)
generateSmall a b g acc
    | a > 10 = (acc, g)
    | b > 10 = generateSmall (a + 1) (-11) g acc
    | otherwise =
        let (chooseMat, g1) = randomDouble g
            (offX, g2) = randomDouble g1
            (offZ, g3) = randomDouble g2
            center = Vec3 (fromIntegral a + 0.9 * offX) 0.2 (fromIntegral b + 0.9 * offZ)
            dist = vecLength (center `vecSub` Vec3 4 0.2 0)
        in if dist <= 0.9
           then generateSmall a (b + 1) g3 acc
           else
               let (sphere, g4) = makeSphere chooseMat center g3
               in generateSmall a (b + 1) g4 (sphere : acc)

makeSphere :: Double -> Point3 -> RNG -> (Sphere, RNG)
makeSphere chooseMat center g
    | chooseMat < 0.8 =
        let (c1, g1) = randomVec3 g
            (c2, g2) = randomVec3 g1
            albedo = c1 `vecMul` c2
        in (Sphere center 0.2 (Lambertian albedo), g2)
    | chooseMat < 0.95 =
        let (albedo, g1) = randomVec3Range 0.5 1.0 g
            (fuzz, g2) = randomDoubleRange 0.0 0.5 g1
        in (Sphere center 0.2 (Metal albedo fuzz), g2)
    | otherwise =
        (Sphere center 0.2 (Dielectric 1.5), g)

main :: IO ()
main = do
    let g0 = mkStdGen 42
        (world, g1) = finalScene g0
        cfg = defaultConfig
            { cfgAspectRatio  = 16.0 / 9.0
            , cfgImageWidth   = 400
            , cfgSamplesPerPixel = 100
            , cfgMaxDepth     = 50
            , cfgVfov         = 20
            , cfgLookFrom     = Vec3 13 2 3
            , cfgLookAt       = Vec3 0 0 0
            , cfgVUp          = Vec3 0 1 0
            , cfgDefocusAngle = 0.6
            , cfgFocusDist    = 10.0
            }
        cam = initCamera cfg

    fileHandle <- openFile "image.ppm" WriteMode
    render cam world fileHandle g1
    hClose fileHandle
