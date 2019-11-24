{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Math3D
import           Mesh
import           PixelOps
import           RayTracer
import           Scene

import           Codec.BMP
import           Data.Colour
import           Data.Yaml     as Yaml
import           Linear.Matrix
import           PLY

main :: IO ()
main = do
  result <- Yaml.decodeFileEither "scenes/world.yaml"
  case result of
    Left e -> putStrLn $ show e
    Right (sd :: SceneDesc)
    --   putStrLn $ show (sCamera sd)
    --   putStrLn $ show (sShadows sd)
    --   putStrLn $ show (sBgColor sd)
    --   putStrLn $ show (sViewPlane sd)
    --   putStrLn $ show (sAmbient sd)
    --   putStrLn $ show (sLights sd)
     -> do
      putStrLn "\n"
      -- putStrLn $ show $ viewMatrix $ sCamera sd
      -- putStrLn $ show $ projectionMatrix $ sViewPlane sd
      let os = sObjects sd
          view = viewMatrix $ sCamera sd
      ts <- sequence $ map (objectDescToTriangles view) os
      let tris = concat ts
    --   putStrLn $ show view
    --   putStrLn $ show os
    --   putStrLn $ show tris
    --   putStrLn $ show $
      -- let worlds = map (worldMatrix . oTransform) os
      -- putStrLn $ show $ worlds
      -- putStrLn "\n"
      -- meshes <- sequence $ map (loadMesh . oPath) os
      -- let meshesW = zipWith applyTransformMesh worlds meshes
      -- putStrLn $ show meshes
      -- putStrLn "\n"
      -- putStrLn $ show meshesW
      let listRGB = traceRays (sViewPlane sd) (sBgColor sd) tris
      let w = vWidth $ sViewPlane sd
      let h = vHeight $ sViewPlane sd
      let bs = replicateBS (w * h) $ rgbToByteString $ sBgColor sd
      let bmp = packRGBA32ToBMP24 w h $ listRgbToByteString listRGB
      writeBMP (sOutputFile sd) bmp
