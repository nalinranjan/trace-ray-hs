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
import           Data.ByteString


data Intersection =
  Intersection
    { iNormal :: V3 Float
    , iView   :: V3 Float
    , iRefl   :: V3 Float
    , iLight  :: V3 Float
    }
  deriving (Eq, Show)


radiance :: Ray -> [Object] -> [LightDesc] -> ByteString -> V3 Float -> ByteString
radiance r objs ls bg eye
  | Prelude.null ds = bg
  | otherwise       = rgbToByteString $ clampRGB $ Prelude.foldr (!+!) (RGB 0 0 0) colors
  where
    ds       = mapMaybe (intersect r) objs
    (t, obj) = Prelude.minimum ds
    mat      = material obj
    intPt    = (rOrigin r) + (toV3 t) * (rDirection r)
    n        = normal intPt obj
    v        = normalize $ eye - intPt
    lrs      = Prelude.map f ls
    colors   = [phong mat (Intersection n v l r) lt | (lt, l, r) <- lrs]
    f lt     = (lt, l, r)
      where (l,r) = getLightV3 intPt n lt

getLightV3 :: V3 Float -> V3 Float -> LightDesc -> (V3 Float, V3 Float)
getLightV3 int n lt = (l, r)
  where l = normalize $ (lPosition lt) - int
        r = reflect (-l) n


mkRay :: Int -> Int -> Float -> Int -> Int -> Ray
mkRay w h d i j = Ray (V3 0 0 0) (normalize (V3 x y z))
  where
    x = (fromIntegral $ - (w `div` 2) + i :: Float) + 0.5
    y = (fromIntegral $ - (h `div` 2) + j :: Float) + 0.5
    z = - d

traceRays :: ViewPlaneDesc -> RGB Float -> [Object] -> [LightDesc] -> V3 Float -> [[ByteString]]
traceRays (ViewPlaneDesc w h d _) bg objs ls eye =
  [ [radiance (mkRay' i j) objs ls bg' eye | i <- [0 .. (w - 1)] ] | j <- [0 .. (h - 1)] ] 
    `using` parList rdeepseq
  where
    mkRay' = mkRay w h d
    bg'    = rgbToByteString bg

phong :: MaterialDesc -> Intersection -> LightDesc -> RGB Float
phong mat int l = clampRGB $ scaleRGB (diffuse + specular) color
  where diffColor = mDiffuseColor mat
        light     = lColor l
        lDotN     = LM.dot (iLight int) (iNormal int)
        rDotV     = LM.dot (iRefl int) (iView int)
        color     = multRGB light diffColor
        diffuse   = lDotN
        specular  = rDotV ** (mAlpha mat)
