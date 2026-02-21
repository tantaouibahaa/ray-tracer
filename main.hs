module Main where

import System.IO
import System.Random (mkStdGen)
import System.Environment (getArgs)
import Vec3
import Hittable
import Texture
import Perlin
import Sphere
import Quad
import BVH
import Camera
import Random

bouncingSpheres :: RNG -> (Hittable, CameraConfig, RNG)
bouncingSpheres g0 =
    let groundMat = Lambertian (CheckerTexture 0.32 (SolidColor (Vec3 0.1 0.25 0.25)) (SolidColor (Vec3 0.95 0.92 0.85)))
        ground = mkSphere (Vec3 0 (-1000) 0) 1000 groundMat
        big1 = mkSphere (Vec3 0 1 0) 1.0 (Dielectric 1.5)
        big2 = mkSphere (Vec3 (-4) 1 0) 1.0 (Lambertian (SolidColor (Vec3 0.55 0.05 0.1)))
        big3 = mkSphere (Vec3 4 1 0) 1.0 (Metal (Vec3 0.92 0.72 0.35) 0.0)
        (smallSpheres, g1) = generateSmall (-11) (-11) g0 []
        allObjects = ground : big1 : big2 : big3 : smallSpheres
        (bvh, g2) = mkBVHNode allObjects g1
        cfg = defaultConfig
            { cfgAspectRatio     = 16.0 / 9.0
            , cfgImageWidth      = 1200
            , cfgSamplesPerPixel = 250
            , cfgMaxDepth        = 50
            , cfgVfov            = 20
            , cfgLookFrom        = Vec3 13 1.8 3
            , cfgLookAt          = Vec3 0 0 0
            , cfgVUp             = Vec3 0 1 0
            , cfgDefocusAngle    = 0.45
            , cfgFocusDist       = 10.0
            , cfgBackground      = Vec3 0.65 0.75 0.95
            }
    in (bvh, cfg, g2)

generateSmall :: Int -> Int -> RNG -> [Hittable] -> ([Hittable], RNG)
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
               let (sphere, g4) = makeSmallSphere chooseMat center g3
               in generateSmall a (b + 1) g4 (sphere : acc)

makeSmallSphere :: Double -> Point3 -> RNG -> (Hittable, RNG)
makeSmallSphere chooseMat center g
    | chooseMat < 0.8 =
        let (c1, g1) = randomVec3 g
            (c2, g2) = randomVec3 g1
            albedo = c1 `vecMul` c2
            center2 = center `vecAdd` Vec3 0 (fst (randomDoubleRange 0 0.5 g2)) 0
            (_, g3) = randomDoubleRange 0 0.5 g2
        in (mkMovingSphere center center2 0.2 (Lambertian (SolidColor albedo)), g3)
    | chooseMat < 0.95 =
        let (albedo, g1) = randomVec3Range 0.5 1.0 g
            (fuzz, g2) = randomDoubleRange 0.0 0.5 g1
        in (mkSphere center 0.2 (Metal albedo fuzz), g2)
    | otherwise =
        (mkSphere center 0.2 (Dielectric 1.5), g)

checkeredSpheres :: RNG -> (Hittable, CameraConfig, RNG)
checkeredSpheres g =
    let checker = CheckerTexture 0.32 (SolidColor (Vec3 0.06 0.08 0.3)) (SolidColor (Vec3 0.9 0.75 0.25))
        s1 = mkSphere (Vec3 0 (-10) 0) 10 (Lambertian checker)
        s2 = mkSphere (Vec3 0 10 0) 10 (Lambertian checker)
        s3 = mkSphere (Vec3 0 0 0) 2.0 (Dielectric 1.5)
        (bvh, g') = mkBVHNode [s1, s2, s3] g
        cfg = defaultConfig
            { cfgAspectRatio     = 16.0 / 9.0
            , cfgImageWidth      = 1200
            , cfgSamplesPerPixel = 200
            , cfgMaxDepth        = 50
            , cfgVfov            = 22
            , cfgLookFrom        = Vec3 11 1.5 3
            , cfgLookAt          = Vec3 0 0 0
            , cfgVUp             = Vec3 0 1 0
            , cfgDefocusAngle    = 0.3
            , cfgFocusDist       = 10.0
            , cfgBackground      = Vec3 0.7 0.8 1.0
            }
    in (bvh, cfg, g')

earth :: RNG -> IO (Hittable, CameraConfig, RNG)
earth g = do
    mImg <- loadImage "earthmap.jpg"
    let tex = case mImg of
            Just img -> ImageTexture img
            Nothing  -> SolidColor (Vec3 0 1 1)
        globe = mkSphere (Vec3 0 0 0) 2 (Lambertian tex)
        pedestal = mkSphere (Vec3 0 (-102) 0) 100 (Metal (Vec3 0.15 0.15 0.18) 0.3)
        moon = mkSphere (Vec3 3.5 1.5 (-1)) 0.5 (Dielectric 1.5)
        (bvh, g') = mkBVHNode [globe, pedestal, moon] g
        cfg = defaultConfig
            { cfgAspectRatio     = 16.0 / 9.0
            , cfgImageWidth      = 1200
            , cfgSamplesPerPixel = 150
            , cfgMaxDepth        = 50
            , cfgVfov            = 25
            , cfgLookFrom        = Vec3 4 3 10
            , cfgLookAt          = Vec3 0 0.5 0
            , cfgVUp             = Vec3 0 1 0
            , cfgDefocusAngle    = 0
            , cfgFocusDist       = 10.0
            , cfgBackground      = Vec3 0.6 0.7 0.9
            }
    return (bvh, cfg, g')

perlinSpheres :: RNG -> (Hittable, CameraConfig, RNG)
perlinSpheres g0 =
    let (perl, g1) = mkPerlin g0
        tex = NoiseTexture perl 9
        s1 = mkSphere (Vec3 0 (-1000) 0) 1000 (Lambertian tex)
        s2 = mkSphere (Vec3 0 2 0) 2 (Lambertian tex)
        s3 = mkSphere (Vec3 (-2.5) 1.2 2) 1.2 (Dielectric 1.5)
        s4 = mkSphere (Vec3 3.5 1.0 1.5) 1.0 (Metal (Vec3 0.95 0.88 0.75) 0.02)
        s5 = mkSphere (Vec3 (-1) 0.4 3.5) 0.4 (Lambertian (SolidColor (Vec3 0.7 0.15 0.08)))
        (bvh, g2) = mkBVHNode [s1, s2, s3, s4, s5] g1
        cfg = defaultConfig
            { cfgAspectRatio     = 16.0 / 9.0
            , cfgImageWidth      = 1200
            , cfgSamplesPerPixel = 200
            , cfgMaxDepth        = 50
            , cfgVfov            = 20
            , cfgLookFrom        = Vec3 12 2.5 4
            , cfgLookAt          = Vec3 0 1 0
            , cfgVUp             = Vec3 0 1 0
            , cfgDefocusAngle    = 0
            , cfgFocusDist       = 10.0
            , cfgBackground      = Vec3 0.7 0.8 1.0
            }
    in (bvh, cfg, g2)

quads :: RNG -> (Hittable, CameraConfig, RNG)
quads g =
    let leftRed     = mkQuad (Vec3 (-3) (-2) 5)  (Vec3 0 0 (-4)) (Vec3 0 4 0) (Lambertian (SolidColor (Vec3 0.8 0.12 0.15)))
        backGreen   = mkQuad (Vec3 (-2) (-2) 0)  (Vec3 4 0 0)    (Vec3 0 4 0) (Lambertian (SolidColor (Vec3 0.12 0.65 0.3)))
        rightBlue   = mkQuad (Vec3 3 (-2) 1)     (Vec3 0 0 4)    (Vec3 0 4 0) (Lambertian (SolidColor (Vec3 0.12 0.18 0.75)))
        upperAmber  = mkQuad (Vec3 (-2) 3 1)     (Vec3 4 0 0)    (Vec3 0 0 4) (Lambertian (SolidColor (Vec3 0.9 0.65 0.08)))
        lowerViolet = mkQuad (Vec3 (-2) (-3) 5)  (Vec3 4 0 0)    (Vec3 0 0 (-4)) (Lambertian (SolidColor (Vec3 0.5 0.2 0.65)))
        glassSph    = mkSphere (Vec3 0 0 3) 1.2 (Dielectric 1.5)
        mirrorSph   = mkSphere (Vec3 (-1.2) (-1.2) 3.5) 0.6 (Metal (Vec3 0.9 0.9 0.95) 0.0)
        (bvh, g') = mkBVHNode [leftRed, backGreen, rightBlue, upperAmber, lowerViolet, glassSph, mirrorSph] g
        cfg = defaultConfig
            { cfgAspectRatio     = 1.0
            , cfgImageWidth      = 1000
            , cfgSamplesPerPixel = 200
            , cfgMaxDepth        = 50
            , cfgVfov            = 80
            , cfgLookFrom        = Vec3 0 0 10
            , cfgLookAt          = Vec3 0 0 0
            , cfgVUp             = Vec3 0 1 0
            , cfgDefocusAngle    = 0
            , cfgFocusDist       = 10.0
            , cfgBackground      = Vec3 0.7 0.8 1.0
            }
    in (bvh, cfg, g')

simpleLight :: RNG -> (Hittable, CameraConfig, RNG)
simpleLight g0 =
    let (perl, g1) = mkPerlin g0
        tex = NoiseTexture perl 4
        s1 = mkSphere (Vec3 0 (-1000) 0) 1000 (Lambertian tex)
        s2 = mkSphere (Vec3 0 2 0) 2 (Lambertian tex)
        glassSph = mkSphere (Vec3 3.5 1 0) 1.0 (Dielectric 1.5)
        diffLight = DiffuseLight (SolidColor (Vec3 6 4 2.5))
        lightQuad = mkQuad (Vec3 2.5 0.5 (-2)) (Vec3 3 0 0) (Vec3 0 3 0) diffLight
        lightSphere = mkSphere (Vec3 0 9 0) 2 (DiffuseLight (SolidColor (Vec3 3 2.2 1.5)))
        (bvh, g2) = mkBVHNode [s1, s2, glassSph, lightQuad, lightSphere] g1
        cfg = defaultConfig
            { cfgAspectRatio     = 16.0 / 9.0
            , cfgImageWidth      = 1200
            , cfgSamplesPerPixel = 300
            , cfgMaxDepth        = 60
            , cfgVfov            = 22
            , cfgLookFrom        = Vec3 22 3 8
            , cfgLookAt          = Vec3 0 2 0
            , cfgVUp             = Vec3 0 1 0
            , cfgDefocusAngle    = 0
            , cfgFocusDist       = 10.0
            , cfgBackground      = Vec3 0 0 0
            }
    in (bvh, cfg, g2)

cornellBox :: RNG -> (Hittable, CameraConfig, RNG)
cornellBox g =
    let indigo = Lambertian (SolidColor (Vec3 0.1 0.06 0.5))
        white  = Lambertian (SolidColor (Vec3 0.73 0.73 0.73))
        green  = Lambertian (SolidColor (Vec3 0.12 0.45 0.15))
        light  = DiffuseLight (SolidColor (Vec3 16 14 10))

        q1 = mkQuad (Vec3 555 0 0) (Vec3 0 555 0) (Vec3 0 0 555) green
        q2 = mkQuad (Vec3 0 0 0) (Vec3 0 555 0) (Vec3 0 0 555) indigo
        q3 = mkQuad (Vec3 343 554 332) (Vec3 (-130) 0 0) (Vec3 0 0 (-105)) light
        q4 = mkQuad (Vec3 0 0 0) (Vec3 555 0 0) (Vec3 0 0 555) white
        q5 = mkQuad (Vec3 555 555 555) (Vec3 (-555) 0 0) (Vec3 0 0 (-555)) white
        q6 = mkQuad (Vec3 0 0 555) (Vec3 555 0 0) (Vec3 0 555 0) white

        box1 = mkRotateY (mkBox (Vec3 0 0 0) (Vec3 165 330 165) white) 15
        box1t = mkTranslate box1 (Vec3 265 0 295)
        glassSph = mkSphere (Vec3 185 82.5 169) 82.5 (Dielectric 1.5)

        (bvh, g') = mkBVHNode [q1, q2, q3, q4, q5, q6, box1t, glassSph] g
        cfg = defaultConfig
            { cfgAspectRatio     = 1.0
            , cfgImageWidth      = 1000
            , cfgSamplesPerPixel = 400
            , cfgMaxDepth        = 50
            , cfgVfov            = 40
            , cfgLookFrom        = Vec3 278 278 (-800)
            , cfgLookAt          = Vec3 278 278 0
            , cfgVUp             = Vec3 0 1 0
            , cfgDefocusAngle    = 0
            , cfgFocusDist       = 10.0
            , cfgBackground      = Vec3 0 0 0
            }
    in (bvh, cfg, g')

cornellSmoke :: RNG -> (Hittable, CameraConfig, RNG)
cornellSmoke g0 =
    let red   = Lambertian (SolidColor (Vec3 0.65 0.05 0.05))
        white = Lambertian (SolidColor (Vec3 0.73 0.73 0.73))
        green = Lambertian (SolidColor (Vec3 0.12 0.45 0.15))
        light = DiffuseLight (SolidColor (Vec3 10 8 5))

        q1 = mkQuad (Vec3 555 0 0) (Vec3 0 555 0) (Vec3 0 0 555) green
        q2 = mkQuad (Vec3 0 0 0) (Vec3 0 555 0) (Vec3 0 0 555) red
        q3 = mkQuad (Vec3 100 554 114) (Vec3 355 0 0) (Vec3 0 0 330) light
        q4 = mkQuad (Vec3 0 555 0) (Vec3 555 0 0) (Vec3 0 0 555) white
        q5 = mkQuad (Vec3 0 0 0) (Vec3 555 0 0) (Vec3 0 0 555) white
        q6 = mkQuad (Vec3 0 0 555) (Vec3 555 0 0) (Vec3 0 555 0) white

        box1 = mkTranslate (mkRotateY (mkBox (Vec3 0 0 0) (Vec3 165 330 165) white) 15) (Vec3 265 0 295)
        box2 = mkTranslate (mkRotateY (mkBox (Vec3 0 0 0) (Vec3 165 165 165) white) (-18)) (Vec3 130 0 65)

        (smoke1, g1) = mkConstantMedium box1 0.015 (Isotropic (SolidColor (Vec3 0.12 0.03 0.35))) g0
        (smoke2, g2) = mkConstantMedium box2 0.015 (Isotropic (SolidColor (Vec3 0.95 0.7 0.15))) g1

        (bvh, g3) = mkBVHNode [q1, q2, q3, q4, q5, q6, smoke1, smoke2] g2
        cfg = defaultConfig
            { cfgAspectRatio     = 1.0
            , cfgImageWidth      = 1000
            , cfgSamplesPerPixel = 350
            , cfgMaxDepth        = 50
            , cfgVfov            = 40
            , cfgLookFrom        = Vec3 278 278 (-800)
            , cfgLookAt          = Vec3 278 278 0
            , cfgVUp             = Vec3 0 1 0
            , cfgDefocusAngle    = 0
            , cfgFocusDist       = 10.0
            , cfgBackground      = Vec3 0 0 0
            }
    in (bvh, cfg, g3)

finalScene :: RNG -> IO (Hittable, CameraConfig, RNG)
finalScene g0 = do
    let ground = Lambertian (SolidColor (Vec3 0.48 0.78 0.38))
        white = Lambertian (SolidColor (Vec3 0.73 0.73 0.73))

        (boxes1, g1) = makeGroundBoxes 0 0 g0 []
        (groundBvh, g2) = mkBVHNode boxes1 g1

        light = mkQuad (Vec3 123 554 147) (Vec3 300 0 0) (Vec3 0 0 265) (DiffuseLight (SolidColor (Vec3 9 9 9)))

        movingSph = mkMovingSphere (Vec3 100 400 200) (Vec3 130 400 200) 50 (Lambertian (SolidColor (Vec3 0.7 0.3 0.1)))
        glassSph = mkSphere (Vec3 260 150 45) 50 (Dielectric 1.5)
        metalSph = mkSphere (Vec3 0 150 145) 50 (Metal (Vec3 0.8 0.8 0.9) 0.0)

        boundary1 = mkSphere (Vec3 360 150 145) 70 (Dielectric 1.5)
        (fog1, g3) = mkConstantMedium boundary1 0.2 (Isotropic (SolidColor (Vec3 0.3 0.15 0.7))) g2

        boundary2 = mkSphere (Vec3 0 0 0) 5000 (Dielectric 1.5)
        (fog2, g4) = mkConstantMedium boundary2 0.0001 (Isotropic (SolidColor (Vec3 0.98 0.92 0.82))) g3

    mImg <- loadImage "earthmap.jpg"
    let earthTex = case mImg of
            Just img -> ImageTexture img
            Nothing  -> SolidColor (Vec3 0 1 1)
        earthSph = mkSphere (Vec3 400 200 400) 100 (Lambertian earthTex)

        (perl, g5) = mkPerlin g4
        noiseSph = mkSphere (Vec3 220 280 300) 80 (Lambertian (NoiseTexture perl 0.2))

        (smallSpheres, g6) = makeSmallSpheresForFinal 0 g5 []
        (smallBvh, g7) = mkBVHNode smallSpheres g6
        smallBvhTranslated = mkTranslate (mkRotateY smallBvh 15) (Vec3 (-100) 270 395)

        allObjects = [groundBvh, light, movingSph, glassSph, metalSph,
                      boundary1, fog1, fog2, earthSph, noiseSph, smallBvhTranslated]
        (bvh, g8) = mkBVHNode allObjects g7

        cfg = defaultConfig
            { cfgAspectRatio     = 1.0
            , cfgImageWidth      = 1000
            , cfgSamplesPerPixel = 400
            , cfgMaxDepth        = 50
            , cfgVfov            = 40
            , cfgLookFrom        = Vec3 478 278 (-600)
            , cfgLookAt          = Vec3 278 278 0
            , cfgVUp             = Vec3 0 1 0
            , cfgDefocusAngle    = 0
            , cfgFocusDist       = 10.0
            , cfgBackground      = Vec3 0 0 0
            }
    return (bvh, cfg, g8)

cornellBoxGlass :: RNG -> (Hittable, CameraConfig, Hittable, RNG)
cornellBoxGlass g =
    let red   = Lambertian (SolidColor (Vec3 0.7 0.08 0.08))
        white = Lambertian (SolidColor (Vec3 0.73 0.73 0.73))
        green = Lambertian (SolidColor (Vec3 0.12 0.45 0.15))
        light = DiffuseLight (SolidColor (Vec3 18 15 10))

        q1 = mkQuad (Vec3 555 0 0) (Vec3 0 0 555) (Vec3 0 555 0) green
        q2 = mkQuad (Vec3 0 0 555) (Vec3 0 0 (-555)) (Vec3 0 555 0) red
        q3 = mkQuad (Vec3 213 554 227) (Vec3 130 0 0) (Vec3 0 0 105) light
        q4 = mkQuad (Vec3 0 555 0) (Vec3 555 0 0) (Vec3 0 0 555) white
        q5 = mkQuad (Vec3 0 0 555) (Vec3 555 0 0) (Vec3 0 0 (-555)) white
        q6 = mkQuad (Vec3 555 0 555) (Vec3 (-555) 0 0) (Vec3 0 555 0) white

        box1 = mkRotateY (mkBox (Vec3 0 0 0) (Vec3 165 330 165) white) 15
        box1t = mkTranslate box1 (Vec3 265 0 295)

        glass = Dielectric 1.5
        glassSphere = mkSphere (Vec3 190 90 190) 90 glass
        goldSphere = mkSphere (Vec3 350 40 350) 40 (Metal (Vec3 0.85 0.7 0.4) 0.05)

        dummyMat = Lambertian (SolidColor (Vec3 0 0 0))
        lightsObj = mkHittableList
            [ mkQuad (Vec3 343 554 332) (Vec3 (-130) 0 0) (Vec3 0 0 (-105)) dummyMat
            , mkSphere (Vec3 190 90 190) 90 dummyMat
            ]

        (bvh, g') = mkBVHNode [q1, q2, q3, q4, q5, q6, box1t, glassSphere, goldSphere] g
        cfg = defaultConfig
            { cfgAspectRatio     = 1.0
            , cfgImageWidth      = 1000
            , cfgSamplesPerPixel = 1000
            , cfgMaxDepth        = 50
            , cfgVfov            = 40
            , cfgLookFrom        = Vec3 278 278 (-800)
            , cfgLookAt          = Vec3 278 278 0
            , cfgVUp             = Vec3 0 1 0
            , cfgDefocusAngle    = 0
            , cfgFocusDist       = 10.0
            , cfgBackground      = Vec3 0 0 0
            }
    in (bvh, cfg, lightsObj, g')

makeGroundBoxes :: Int -> Int -> RNG -> [Hittable] -> ([Hittable], RNG)
makeGroundBoxes i j g acc
    | i >= 20 = (acc, g)
    | j >= 20 = makeGroundBoxes (i + 1) 0 g acc
    | otherwise =
        let ground = Lambertian (SolidColor (Vec3 0.48 0.78 0.38))
            w = 100.0
            x0 = -1000.0 + fromIntegral i * w
            z0 = -1000.0 + fromIntegral j * w
            (y1, g') = randomDoubleRange 1 101 g
            x1 = x0 + w
            z1 = z0 + w
            box = mkBox (Vec3 x0 0 z0) (Vec3 x1 y1 z1) ground
        in makeGroundBoxes i (j + 1) g' (box : acc)

makeSmallSpheresForFinal :: Int -> RNG -> [Hittable] -> ([Hittable], RNG)
makeSmallSpheresForFinal n g acc
    | n >= 1000 = (acc, g)
    | otherwise =
        let white = Lambertian (SolidColor (Vec3 0.73 0.73 0.73))
            (v, g') = randomVec3Range 0 165 g
        in makeSmallSpheresForFinal (n + 1) g' (mkSphere v 10 white : acc)

renderScene :: String -> Camera -> Hittable -> Maybe Hittable -> RNG -> IO ()
renderScene outFile cam world mLights g = do
    fileHandle <- openFile outFile WriteMode
    render cam world mLights fileHandle g
    hClose fileHandle

main :: IO ()
main = do
    args <- getArgs
    let sceneNum = case args of
            (s:_) -> read s :: Int
            _     -> 1
        outFile = case args of
            (_:f:_) -> f
            _       -> "image.ppm"
        g0 = mkStdGen 42

    case sceneNum of
        2 -> do
            let (world, cfg, g1) = checkeredSpheres g0
            renderScene outFile (initCamera cfg) world Nothing g1

        3 -> do
            (world, cfg, g1) <- earth g0
            renderScene outFile (initCamera cfg) world Nothing g1

        4 -> do
            let (world, cfg, g1) = perlinSpheres g0
            renderScene outFile (initCamera cfg) world Nothing g1

        5 -> do
            let (world, cfg, g1) = quads g0
            renderScene outFile (initCamera cfg) world Nothing g1

        6 -> do
            let (world, cfg, g1) = simpleLight g0
            renderScene outFile (initCamera cfg) world Nothing g1

        7 -> do
            let (world, cfg, g1) = cornellBox g0
                dummyMat = Lambertian (SolidColor (Vec3 0 0 0))
                lights = mkHittableList
                    [ mkQuad (Vec3 343 554 332) (Vec3 (-130) 0 0) (Vec3 0 0 (-105)) dummyMat
                    , mkSphere (Vec3 185 82.5 169) 82.5 dummyMat
                    ]
            renderScene outFile (initCamera cfg) world (Just lights) g1

        8 -> do
            let (world, cfg, g1) = cornellSmoke g0
            renderScene outFile (initCamera cfg) world Nothing g1

        9 -> do
            (world, cfg, g1) <- finalScene g0
            renderScene outFile (initCamera cfg) world Nothing g1

        10 -> do
            let (world, cfg, lights, g1) = cornellBoxGlass g0
            renderScene outFile (initCamera cfg) world (Just lights) g1

        _ -> do
            let (world, cfg, g1) = bouncingSpheres g0
            renderScene outFile (initCamera cfg) world Nothing g1
