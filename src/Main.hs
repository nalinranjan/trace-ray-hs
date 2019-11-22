{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Math3D
import           Scene

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
      putStrLn $ show (sObjects sd)
      putStrLn "\n"
      putStrLn $ show $ viewMatrix $ sCamera sd
      putStrLn $ show $ projectionMatrix $ sViewPlane sd
