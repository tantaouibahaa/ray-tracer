module Camera where

import System.IO
import Vec3
import Ray
import Interval
import Hittable
import Sphere
import Color
import Material
import Random

data CameraConfig = CameraConfig
    { cfgAspectRatio  :: !Double
    , cfgImageWidth   :: !Int
    , cfgSamplesPerPixel :: !Int
    , cfgMaxDepth     :: !Int
    , cfgVfov         :: !Double
    , cfgLookFrom     :: !Point3
    , cfgLookAt       :: !Point3
    , cfgVUp          :: !Vec3
    , cfgDefocusAngle :: !Double
    , cfgFocusDist    :: !Double
    }

data Camera = Camera
    { camImageWidth   :: !Int
    , camImageHeight  :: !Int
    , camCenter       :: !Point3
    , camPixel00Loc   :: !Point3
    , camPixelDeltaU  :: !Vec3
    , camPixelDeltaV  :: !Vec3
    , camSamplesPerPixel :: !Int
    , camPixelSampleScale :: !Double
    , camMaxDepth     :: !Int
    , camDefocusDiskU :: !Vec3
    , camDefocusDiskV :: !Vec3
    , camDefocusAngle :: !Double
    }

defaultConfig :: CameraConfig
defaultConfig = CameraConfig
    { cfgAspectRatio  = 16.0 / 9.0
    , cfgImageWidth   = 400
    , cfgSamplesPerPixel = 10
    , cfgMaxDepth     = 10
    , cfgVfov         = 90
    , cfgLookFrom     = Vec3 0 0 0
    , cfgLookAt       = Vec3 0 0 (-1)
    , cfgVUp          = Vec3 0 1 0
    , cfgDefocusAngle = 0
    , cfgFocusDist    = 10
    }

initCamera :: CameraConfig -> Camera
initCamera cfg =
    let imageWidth = cfgImageWidth cfg
        imageHeight = max 1 (floor (fromIntegral imageWidth / cfgAspectRatio cfg))
        center = cfgLookFrom cfg
        samplesPerPixel = cfgSamplesPerPixel cfg
        pixelSampleScale = 1.0 / fromIntegral samplesPerPixel

        theta = cfgVfov cfg * pi / 180.0
        h = tan (theta / 2.0)
        viewportHeight = 2.0 * h * cfgFocusDist cfg
        viewportWidth = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)

        w = unitVector (cfgLookFrom cfg `vecSub` cfgLookAt cfg)
        u = unitVector (cross (cfgVUp cfg) w)
        v = cross w u

        viewportU = vecScale viewportWidth u
        viewportV = vecScale (-viewportHeight) v

        pixelDeltaU = vecDiv viewportU (fromIntegral imageWidth)
        pixelDeltaV = vecDiv viewportV (fromIntegral imageHeight)

        viewportUpperLeft = center
            `vecSub` vecScale (cfgFocusDist cfg) w
            `vecSub` vecDiv viewportU 2
            `vecSub` vecDiv viewportV 2
        pixel00Loc = viewportUpperLeft
            `vecAdd` vecScale 0.5 (pixelDeltaU `vecAdd` pixelDeltaV)

        defocusRadius = cfgFocusDist cfg * tan (cfgDefocusAngle cfg / 2.0 * pi / 180.0)
        defocusDiskU = vecScale defocusRadius u
        defocusDiskV = vecScale defocusRadius v

    in Camera
        { camImageWidth   = imageWidth
        , camImageHeight  = imageHeight
        , camCenter       = center
        , camPixel00Loc   = pixel00Loc
        , camPixelDeltaU  = pixelDeltaU
        , camPixelDeltaV  = pixelDeltaV
        , camSamplesPerPixel = samplesPerPixel
        , camPixelSampleScale = pixelSampleScale
        , camMaxDepth     = cfgMaxDepth cfg
        , camDefocusDiskU = defocusDiskU
        , camDefocusDiskV = defocusDiskV
        , camDefocusAngle = cfgDefocusAngle cfg
        }

render :: Camera -> [Sphere] -> Handle -> RNG -> IO ()
render cam world handle g0 = do
    hPutStrLn handle "P3"
    hPutStrLn handle (show (camImageWidth cam) ++ " " ++ show (camImageHeight cam))
    hPutStrLn handle "255"
    let rows = [0 .. camImageHeight cam - 1]
        cols = [0 .. camImageWidth cam - 1]
    go rows cols g0
  where
    go [] _ g = do
        hPutStrLn stderr "\rDone.                    "
        let _ = g
        return ()
    go (row:restRows) cols g = do
        hPutStr stderr ("\rScanlines remaining: " ++ show (camImageHeight cam - row) ++ " ")
        hFlush stderr
        g' <- goRow cols row g
        go restRows cols g'
    goRow [] _ g = return g
    goRow (col:restCols) row g = do
        let (pixelColor, g') = samplePixel cam world row col g
        writeColor handle pixelColor
        goRow restCols row g'

samplePixel :: Camera -> [Sphere] -> Int -> Int -> RNG -> (Vec3, RNG)
samplePixel cam world row col g0 =
    let n = camSamplesPerPixel cam
        go 0 acc g = (vecScale (camPixelSampleScale cam) acc, g)
        go i acc g =
            let (ray, g') = getRay cam row col g
                (color, g'') = rayColor cam world ray (camMaxDepth cam) g'
            in go (i - 1) (acc `vecAdd` color) g''
    in go n (Vec3 0 0 0) g0

getRay :: Camera -> Int -> Int -> RNG -> (Ray, RNG)
getRay cam row col g0 =
    let (sx, g1) = randomDouble g0
        (sy, g2) = randomDouble g1
        offsetX = sx - 0.5
        offsetY = sy - 0.5
        pixelSample = camPixel00Loc cam
            `vecAdd` vecScale (fromIntegral col + offsetX) (camPixelDeltaU cam)
            `vecAdd` vecScale (fromIntegral row + offsetY) (camPixelDeltaV cam)
        (origin, g3) = if camDefocusAngle cam <= 0
            then (camCenter cam, g2)
            else defocusDiskSample cam g2
        direction = pixelSample `vecSub` origin
    in (Ray origin direction, g3)

defocusDiskSample :: Camera -> RNG -> (Point3, RNG)
defocusDiskSample cam g =
    let (p, g') = randomInUnitDisk g
        sample = camCenter cam
            `vecAdd` vecScale (vecX p) (camDefocusDiskU cam)
            `vecAdd` vecScale (vecY p) (camDefocusDiskV cam)
    in (sample, g')

rayColor :: Camera -> [Sphere] -> Ray -> Int -> RNG -> (Vec3, RNG)
rayColor _ _ _ 0 g = (Vec3 0 0 0, g)
rayColor cam world ray depth g =
    case hitWorld world hitSphere ray (Interval 0.001 (1/0)) of
        Just rec ->
            case scatter (hitMaterial rec) ray rec g of
                Just (attenuation, scattered, g') ->
                    let (col, g'') = rayColor cam world scattered (depth - 1) g'
                    in (attenuation `vecMul` col, g'')
                Nothing -> (Vec3 0 0 0, g)
        Nothing ->
            let unitDir = unitVector (rayDirection ray)
                a = 0.5 * (vecY unitDir + 1.0)
                white = Vec3 1.0 1.0 1.0
                blue  = Vec3 0.5 0.7 1.0
            in (vecAdd (vecScale (1.0 - a) white) (vecScale a blue), g)
