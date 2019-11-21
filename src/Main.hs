{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Scene

import Codec.BMP
import PLY
import Linear.Matrix
import Data.Yaml as Yaml
-- import Data.Text
import Data.Colour


main :: IO ()
main = do result <- Yaml.decodeFileEither "scenes/cube.yaml"
          case result of 
            Left e -> putStrLn $ show e
            Right (sd :: SceneDesc) -> do putStrLn $ show (sCamera sd)
                                          putStrLn $ show (sShadows sd)
                                          putStrLn $ show (sBgColor sd)
                                          putStrLn $ show (sViewPlane sd)
                                          putStrLn $ show (sAmbient sd)
                                          putStrLn $ show (sLights sd)
                                          putStrLn $ show (sObjects sd)
            
