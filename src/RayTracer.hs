module RayTracer where

import           Objects
import           Scene
import           PixelOps
import           MatrixMath

import           Data.Colour.SRGB.Linear
-- import           Data.Colour.Internal
-- import           Data.Colour.RGBSpace
import           Data.Maybe
import           Linear.V3
import           Linear.Metric as LM
import           Control.Parallel.Strategies
import qualified Data.ByteString as BS


data Intersection =
  Intersection
    { iNormal :: V3 Float
    , iView   :: V3 Float
    , iRefl   :: V3 Float
    , iLight  :: V3 Float
    }
  deriving (Eq, Show)


radiance :: Ray -> [Object] -> [LightDesc] -> BS.ByteString -> V3 Float -> RGB Float -> BS.ByteString
radiance r objs ls bg eye amb
  | Prelude.null ds = bg
  | otherwise       = rgbToByteString $ clampRGB $ totalRad
  where
    ds       = mapMaybe (intersect r) objs
    (t, obj) = Prelude.minimum ds
    mat      = material obj
    intPt    = (rOrigin r) + (toV3 t) * (rDirection r)
    n        = normal intPt obj
    v        = normalize $ eye - intPt
    lrs      = mapMaybe calcIntersection ls
    rads     = [phong mat (Intersection n v l r) lt | (lt, l, r) <- lrs]
    totalRad = amb !+! Prelude.foldr (!+!) (RGB 0 0 0) rads
    calcIntersection lt = if inShadow intPt objs lt
                          then Nothing
                          else Just (lt, l, r)
      where (l,r) = getLightV3 intPt n lt

getLightV3 :: V3 Float -> V3 Float -> LightDesc -> (V3 Float, V3 Float)
getLightV3 int n lt = (l, r)
  where l = normalize $ (lPosition lt) - int
        r = reflect (-l) n

inShadow :: V3 Float -> [Object] -> LightDesc -> Bool
inShadow pt objs lt = any isJust $ fmap (intersect r) objs
  where r = Ray pt $ normalize $ lPosition lt - pt

mkRay :: Int -> Int -> Float -> Int -> Int -> Ray
mkRay w h d i j = Ray (V3 0 0 0) (normalize (V3 x y z))
  where
    x = (fromIntegral $ - (w `div` 2) + i :: Float) + 0.5
    y = (fromIntegral $ - (h `div` 2) + j :: Float) + 0.5
    z = - d

-- traceRays :: ViewPlaneDesc -> RGB Float -> [Object] -> [LightDesc] -> V3 Float -> [[ByteString]]
traceRays :: SceneDesc -> [Object] -> [LightDesc] -> [[BS.ByteString]]
-- traceRays (ViewPlaneDesc w h d _) bg objs ls eye =
traceRays sd objs ls =
  [ [radiance (mkRay' i j) objs ls bg' eye amb | i <- [0 .. (w - 1)] ] | j <- [0 .. (h - 1)] ] 
    `using` parList rdeepseq
  where
    (ViewPlaneDesc w h d _) = sViewPlane sd
    bg'                     = rgbToByteString $ sBgColor sd
    eye                     = cEyePoint $ sCamera sd
    mkRay'                  = mkRay w h d
    amb                     = sAmbient sd
    -- bg'                     = rgbToByteString bg

phong :: MaterialDesc -> Intersection -> LightDesc -> RGB Float
phong mat int l = clampRGB $ scaleRGB (diffuse + specular) color
  where diffColor = mDiffuseColor mat
        light     = lColor l
        lDotN     = LM.dot (iLight int) (iNormal int)
        rDotV     = LM.dot (iRefl int) (iView int)
        color     = multRGB light diffColor
        diffuse   = lDotN
        specular  = rDotV ** (mAlpha mat)
