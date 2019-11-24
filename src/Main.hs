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
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let sceneFile = case args of
                    []    -> "scenes/cube.yaml"
                    (f:_) -> f
  -- putStrLn $ show args
  putStrLn sceneFile
  result <- Yaml.decodeFileEither sceneFile
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
      let os = sObjects sd
          view = viewMatrix $ sCamera sd
      ts <- sequence $ map (objectDescToTriangles view) os
      let tris = concat ts
      let listRGB = traceRays (sViewPlane sd) (sBgColor sd) tris
      let w = vWidth $ sViewPlane sd
      let h = vHeight $ sViewPlane sd
      let bmp = packRGBA32ToBMP24 w h $ listRgbToByteString listRGB
      writeBMP (sOutputFile sd) bmp
