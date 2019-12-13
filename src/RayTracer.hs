module RayTracer where

import           Objects
import           Scene
import           PixelOps

import           Data.Colour.SRGB.Linear
import           Data.Maybe
import           Linear.V3
import           Linear.Metric
import           Control.Parallel.Strategies
import           Data.ByteString


-- radiance :: Ray -> [Triangle] -> ByteString -> ByteString
radiance :: Ray -> [Object] -> ByteString -> ByteString
radiance r objs bg
  | Prelude.null ds = bg
  | otherwise       = rgbToByteString $ mDiffuseColor $ material $ snd $ Prelude.minimum ds
  -- | otherwise       = rgbToByteString $ mDiffuseColor $ tMat $ snd $ Prelude.minimum ds
  where
    ds = mapMaybe (intersect r) objs

mkRay :: Int -> Int -> Float -> Int -> Int -> Ray
mkRay w h d i j = Ray (V3 0 0 0) (normalize (V3 x y z))
  where
    x = (fromIntegral $ - (w `div` 2) + i :: Float) + 0.5
    y = (fromIntegral $ - (h `div` 2) + j :: Float) + 0.5
    z = - d

-- traceRays :: ViewPlaneDesc -> RGB Float -> [Triangle] -> [[ByteString]]
traceRays :: ViewPlaneDesc -> RGB Float -> [Object] -> [[ByteString]]
traceRays (ViewPlaneDesc w h d _) bg objs =
  [ [radiance (mkRay' i j) objs bg' | i <- [0 .. (w - 1)] ] | j <- [0 .. (h - 1)] ] `using` parList rdeepseq
  where
    mkRay' = mkRay w h d
    bg'    = rgbToByteString bg
