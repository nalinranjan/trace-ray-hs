module RayTracer where

import Mesh
import Scene
import Math3D

import Data.Maybe


radiance :: Ray -> [Triangle] -> RGB Float -> RGB Float
radiance r tris bg | ds == []  = bg
                   | otherwise = mDiffuseColor $ tMat $ snd $ minimum ds
  where ds = mapMaybe intersect tris

mkRay :: ViewPlaneDesc -> V3 Float Int -> Int -> Int -> Int -> V3 Float -> Float 

traceRays :: ViewPlaneDesc -> CameraDesc -> RGB Float -> [Triangle] -> [RGB Float]
traceRays (ViewPlaneDesc w h d _) (CameraDesc cPos _ _) bg tris = 
  [radiance (mkRay) tris bg | i <- [0..(w-1)], j <- [0..(h-1)]]
    where mkRay' = mkRay w h d 