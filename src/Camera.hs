module Camera where

import System.IO
import Vec3
import Ray
import Interval
import Hittable
import Color
import Material
import Random
import PDF

data CameraConfig = CameraConfig
    { cfgAspectRatio     :: !Double
    , cfgImageWidth      :: !Int
    , cfgSamplesPerPixel :: !Int
    , cfgMaxDepth        :: !Int
    , cfgVfov            :: !Double
    , cfgLookFrom        :: !Point3
    , cfgLookAt          :: !Point3
    , cfgVUp             :: !Vec3
    , cfgDefocusAngle    :: !Double
    , cfgFocusDist       :: !Double
    , cfgBackground      :: !Vec3
    }

data Camera = Camera
    { camImageWidth        :: !Int
    , camImageHeight       :: !Int
    , camCenter            :: !Point3
    , camPixel00Loc        :: !Point3
    , camPixelDeltaU       :: !Vec3
    , camPixelDeltaV       :: !Vec3
    , camSqrtSpp           :: !Int
    , camRecipSqrtSpp      :: !Double
    , camPixelSampleScale  :: !Double
    , camMaxDepth          :: !Int
    , camDefocusDiskU      :: !Vec3
    , camDefocusDiskV      :: !Vec3
    , camDefocusAngle      :: !Double
    , camBackground        :: !Vec3
    }

defaultConfig :: CameraConfig
defaultConfig = CameraConfig
    { cfgAspectRatio     = 16.0 / 9.0
    , cfgImageWidth      = 400
    , cfgSamplesPerPixel = 10
    , cfgMaxDepth        = 10
    , cfgVfov            = 90
    , cfgLookFrom        = Vec3 0 0 0
    , cfgLookAt          = Vec3 0 0 (-1)
    , cfgVUp             = Vec3 0 1 0
    , cfgDefocusAngle    = 0
    , cfgFocusDist       = 10
    , cfgBackground      = Vec3 0.7 0.8 1.0
    }

initCamera :: CameraConfig -> Camera
initCamera cfg =
    let imageWidth = cfgImageWidth cfg
        imageHeight = max 1 (floor (fromIntegral imageWidth / cfgAspectRatio cfg))
        center = cfgLookFrom cfg

        sqrtSpp = max 1 (floor (sqrt (fromIntegral (cfgSamplesPerPixel cfg) :: Double)))
        pixelSampleScale = 1.0 / fromIntegral (sqrtSpp * sqrtSpp)
        recipSqrtSpp = 1.0 / fromIntegral sqrtSpp

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
        { camImageWidth       = imageWidth
        , camImageHeight      = imageHeight
        , camCenter           = center
        , camPixel00Loc       = pixel00Loc
        , camPixelDeltaU      = pixelDeltaU
        , camPixelDeltaV      = pixelDeltaV
        , camSqrtSpp          = sqrtSpp
        , camRecipSqrtSpp     = recipSqrtSpp
        , camPixelSampleScale = pixelSampleScale
        , camMaxDepth         = cfgMaxDepth cfg
        , camDefocusDiskU     = defocusDiskU
        , camDefocusDiskV     = defocusDiskV
        , camDefocusAngle     = cfgDefocusAngle cfg
        , camBackground       = cfgBackground cfg
        }

render :: Camera -> Hittable -> Maybe Hittable -> Handle -> RNG -> IO ()
render cam world mLights handle g0 = do
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
        let (pixelColor, g') = samplePixel cam world mLights row col g
        writeColor handle pixelColor
        goRow restCols row g'

samplePixel :: Camera -> Hittable -> Maybe Hittable -> Int -> Int -> RNG -> (Vec3, RNG)
samplePixel cam world mLights row col g0 =
    let sqrtSpp = camSqrtSpp cam
        go sj si acc g
            | sj >= sqrtSpp = (vecScale (camPixelSampleScale cam) acc, g)
            | si >= sqrtSpp = go (sj + 1) 0 acc g
            | otherwise =
                let (ray, g') = getRay cam row col si sj g
                    (color, g'') = rayColor cam world mLights ray (camMaxDepth cam) g'
                in go sj (si + 1) (acc `vecAdd` color) g''
    in go 0 0 (Vec3 0 0 0) g0

getRay :: Camera -> Int -> Int -> Int -> Int -> RNG -> (Ray, RNG)
getRay cam row col si sj g0 =
    let (sx, g1) = randomDouble g0
        (sy, g2) = randomDouble g1
        recipSqrt = camRecipSqrtSpp cam
        px = (fromIntegral si + sx) * recipSqrt - 0.5
        py = (fromIntegral sj + sy) * recipSqrt - 0.5
        pixelSample = camPixel00Loc cam
            `vecAdd` vecScale (fromIntegral col + px) (camPixelDeltaU cam)
            `vecAdd` vecScale (fromIntegral row + py) (camPixelDeltaV cam)
        (origin, g3) = if camDefocusAngle cam <= 0
            then (camCenter cam, g2)
            else defocusDiskSample cam g2
        direction = pixelSample `vecSub` origin
        (tm, g4) = randomDouble g3
    in (Ray origin direction tm, g4)

defocusDiskSample :: Camera -> RNG -> (Point3, RNG)
defocusDiskSample cam g =
    let (p, g') = randomInUnitDisk g
        sample = camCenter cam
            `vecAdd` vecScale (vecX p) (camDefocusDiskU cam)
            `vecAdd` vecScale (vecY p) (camDefocusDiskV cam)
    in (sample, g')

rayColor :: Camera -> Hittable -> Maybe Hittable -> Ray -> Int -> RNG -> (Vec3, RNG)
rayColor _ _ _ _ 0 g = (Vec3 0 0 0, g)
rayColor cam world mLights ray depth g =
    case hittableHit world ray (Interval 0.001 (1/0)) of
        Nothing -> (camBackground cam, g)
        Just rec ->
            let colorEmitted = emitted (hitMaterial rec) ray rec (hitU rec) (hitV rec) (hitPoint rec)
            in case scatter (hitMaterial rec) ray rec g of
                Nothing -> (colorEmitted, g)
                Just (SpecularScatter attn specRay, g') ->
                    let (col, g'') = rayColor cam world mLights specRay (depth - 1) g'
                    in (colorEmitted `vecAdd` (attn `vecMul` col), g'')
                Just (DiffuseScatter attn surfacePdf, g') ->
                    let finalPdf = case mLights of
                            Nothing -> surfacePdf
                            Just lights -> mkMixturePdf (mkHittablePdf lights (hitPoint rec)) surfacePdf
                        (dir, g'') = pdfGenerate finalPdf g'
                        scattered = Ray (hitPoint rec) dir (rayTime ray)
                        pdfVal = pdfValue finalPdf dir
                        scatPdf = scatteringPdf (hitMaterial rec) ray rec scattered
                        (col, g''') = rayColor cam world mLights scattered (depth - 1) g''
                        colorScatter = if pdfVal > 1e-10
                            then vecScale (scatPdf / pdfVal) (attn `vecMul` col)
                            else Vec3 0 0 0
                    in (colorEmitted `vecAdd` colorScatter, g''')
