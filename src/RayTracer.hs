module RayTracer where

import           Objects
import           Scene
import           PixelOps

import           Data.Colour.SRGB.Linear
import           Data.Maybe
import           Linear.V3
import           Control.Parallel.Strategies
import           Data.ByteString


radiance :: Ray -> [Triangle] -> ByteString -> ByteString
radiance r tris bg
  | ds == []  = bg
  | otherwise = rgbToByteString $ mDiffuseColor $ tMat $ snd $ Prelude.minimum ds
  where
    ds = mapMaybe (intersect r) tris

mkRay :: Int -> Int -> Float -> Int -> Int -> Ray
mkRay w h d i j = Ray (V3 0 0 0) (V3 x y z)
  where
    x = (fromIntegral $ - (w `div` 2) + i :: Float) + 0.5
    y = (fromIntegral $ - (h `div` 2) + j :: Float) + 0.5
    z = - d

traceRays :: ViewPlaneDesc -> RGB Float -> [Triangle] -> [[ByteString]]
traceRays (ViewPlaneDesc w h d _) bg tris =
  -- [radiance (mkRay' i j) tris bg' | j <- [0 .. (h - 1)], i <- [0 .. (w - 1)]] `using` parList rdeepseq
  [ [radiance (mkRay' i j) tris bg' | i <- [0 .. (w - 1)] ] | j <- [0 .. (h - 1)] ] `using` parList rdeepseq
  where
    mkRay' = mkRay w h d
    bg'    = rgbToByteString bg
