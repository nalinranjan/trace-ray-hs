module RayTracer where

import           Objects
import           Scene

import           Data.Colour.SRGB.Linear
import           Data.Maybe
import           Linear.V3


radiance :: Ray -> [Triangle] -> RGB Float -> RGB Float
radiance r tris bg
  | ds == [] = bg
  | otherwise = mDiffuseColor $ tMat $ snd $ minimum ds
  where
    ds = mapMaybe (intersect r) tris

mkRay :: Int -> Int -> Float -> Int -> Int -> Ray
mkRay w h d i j = Ray (V3 0 0 0) (V3 x y z)
  where
    x = (fromIntegral $ - ( w `div` 2) + i :: Float) + 0.5
    y = (fromIntegral $ - ( h `div` 2) + j :: Float) + 0.5
    z = - d

traceRays :: ViewPlaneDesc -> RGB Float -> [Triangle] -> [RGB Float]
traceRays (ViewPlaneDesc w h d _) bg tris =
  [radiance (mkRay' i j) tris bg | j <- [0 .. (h - 1)], i <- [0 .. (w - 1)]]
  where
    mkRay' = mkRay w h d
