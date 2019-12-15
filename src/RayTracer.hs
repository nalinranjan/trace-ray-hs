module RayTracer where

import           Objects
import           Scene
import           PixelOps
import           MatrixMath

import           Data.Colour.SRGB.Linear
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


radiance :: Int -> Ray -> [Object] -> [LightDesc] -> RGB Float -> V3 Float -> RGB Float -> RGB Float
radiance depth ray objs ls bg eye amb
  | Prelude.null ds = bg
  | otherwise       = let c = totalRad
                          ref = if depth > 0 && kr > 0
                                then scaleRGB kr $ radiance (pred depth) refRay objs ls bg eye amb
                                else RGB 0 0 0
                          trans = if depth > 0 && kt > 0
                                  then scaleRGB kt $ radiance (pred depth) transRay objs ls bg eye amb
                                  else RGB 0 0 0
                      in c !+! ref !+! trans
                      -- in c
  where
    ds          = mapMaybe (intersect ray) objs
    (t, obj)    = Prelude.minimum ds
    mat         = material obj
    kr          = mKr mat
    kt          = mKt mat
    u1          = rIoR ray
    objIoR      = mIoR mat
    u2          = if u1 == objIoR then 1 else objIoR
    intPt       = (rOrigin ray) + (toV3 t) * (rDirection ray)
    n           = normal intPt obj
    v           = normalize $ eye - intPt
    lrs         = mapMaybe calcIntersection ls
    rads        = [phong mat (Intersection n v l r) lt | (lt, l, r) <- lrs]
    totalRad    = amb !+! Prelude.foldr (!+!) (RGB 0 0 0) rads
    refRay      = Ray intPt (reflect (rDirection ray) n) u1
    (tDir, tir) = transmit (rDirection ray) n u1 u2
    transRay    = Ray intPt tDir $ if tir then u1 else u2
    calcIntersection lt = if inShadow intPt objs lt
                          then Nothing
                          else Just (lt, l, r)
      where (l,r) = getLightV3 intPt n lt

getLightV3 :: V3 Float -> V3 Float -> LightDesc -> (V3 Float, V3 Float)
getLightV3 int n lt = (l, r)
  where l = normalize $ lPosition lt - int
        r = reflect (-l) n

inShadow :: V3 Float -> [Object] -> LightDesc -> Bool
inShadow pt objs lt = any isJust $ fmap (intersect r) objs
  where r = Ray pt (normalize (lPosition lt - pt)) 1

mkRay :: Int -> Int -> Float -> Int -> Int -> Ray
mkRay w h d i j = Ray (V3 0 0 0) (normalize (V3 x y z)) 1
  where
    x = (fromIntegral $ - (w `div` 2) + i :: Float) + 0.5
    y = (fromIntegral $ - (h `div` 2) + j :: Float) + 0.5
    z = - d

-- traceRays :: ViewPlaneDesc -> RGB Float -> [Object] -> [LightDesc] -> V3 Float -> [[ByteString]]
traceRays :: SceneDesc -> [Object] -> [LightDesc] -> [[BS.ByteString]]
-- traceRays (ViewPlaneDesc w h d _) bg objs ls eye =
traceRays sd objs ls =
  [ [rgbToByteString $ clampRGB $ radiance mdep (mkRay' i j) objs ls bg eye amb
    | i <- [0 .. (w - 1)] ] | j <- [0 .. (h - 1)] ] 
    `using` parList rdeepseq
  where
    (ViewPlaneDesc w h d mdep) = sViewPlane sd
    bg                         = sBgColor sd
    eye                        = cEyePoint $ sCamera sd
    mkRay'                     = mkRay w h d
    amb                        = sAmbient sd

phong :: MaterialDesc -> Intersection -> LightDesc -> RGB Float
-- phong mat int l = clampRGB $ scaleRGB (diffuse + specular) color
phong mat int l = scaleRGB (diffuse + specular) color
  where diffColor = mDiffuseColor mat
        light     = lColor l
        lDotN     = LM.dot (iLight int) (iNormal int)
        rDotV     = LM.dot (iRefl int) (iView int)
        color     = multRGB light diffColor
        diffuse   = lDotN
        specular  = rDotV ** (mAlpha mat)
