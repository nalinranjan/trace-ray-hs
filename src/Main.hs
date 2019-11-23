{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Math3D
import           Scene
import           Mesh
import           PixelOps

import           Codec.BMP
import           Data.Colour
import           Data.Yaml     as Yaml
import           Linear.Matrix
import           PLY

main :: IO ()
main = do
  result <- Yaml.decodeFileEither "scenes/cube.yaml"
  case result of
    Left e -> putStrLn $ show e
    Right (sd :: SceneDesc) -> do
    --   putStrLn $ show (sCamera sd)
    --   putStrLn $ show (sShadows sd)
    --   putStrLn $ show (sBgColor sd)
    --   putStrLn $ show (sViewPlane sd)
    --   putStrLn $ show (sAmbient sd)
    --   putStrLn $ show (sLights sd)
      putStrLn "\n"
      -- putStrLn $ show $ viewMatrix $ sCamera sd
      -- putStrLn $ show $ projectionMatrix $ sViewPlane sd
      let os   = sObjects sd
          view = viewMatrix $ sCamera sd
      ts <- sequence $ map (objectDescToTriangles view) os
      let tris = concat ts
      putStrLn $ show view
      putStrLn $ show os
      putStrLn $ show tris
      -- let worlds = map (worldMatrix . oTransform) os
      -- putStrLn $ show $ worlds
      -- putStrLn "\n"
      -- meshes <- sequence $ map (loadMesh . oPath) os
      -- let meshesW = zipWith applyTransformMesh worlds meshes
      -- putStrLn $ show meshes
      -- putStrLn "\n"
      -- putStrLn $ show meshesW
      let w = vWidth $ sViewPlane sd
      let h = vHeight $ sViewPlane sd
      let bs = replicateBS (w*h) $ rgbToByteString $ sBgColor sd
      let bmp = packRGBA32ToBMP24 w h bs
      writeBMP (sOutputFile sd) bmp
      
      
      
