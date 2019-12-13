{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           MatrixMath
import           Mesh
import           PixelOps
import           RayTracer
import           Scene

import           Codec.BMP
import           Data.Yaml     as Yaml
import           System.Environment
import qualified Data.ByteString as BS


main :: IO ()
main = do
  args <- getArgs
  let sceneFile = case args of
                    []    -> "scenes/cube.yaml"
                    (f:_) -> f
  putStrLn ("Using scene file: " ++ sceneFile)
  result <- Yaml.decodeFileEither sceneFile
  case result of
    Left e -> putStrLn $ show e
    Right (sd :: SceneDesc)
     -> do
      let os   = sObjects sd
          view = viewMatrix $ sCamera sd
      -- ts <- sequence $ map (objectDescToTriangles view) os
      objs <- objectDescsToObjects view os
      let listBS  = traceRays (sViewPlane sd) (sBgColor sd) objs
          w       = vWidth $ sViewPlane sd
          h       = vHeight $ sViewPlane sd
          -- bmp     = packRGBA32ToBMP24 w h $ listRgbToByteString listRGB
          bmp     = packRGBA32ToBMP24 w h $ BS.concat $ map BS.concat listBS
      writeBMP (sOutputFile sd) bmp
