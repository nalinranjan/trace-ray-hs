{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Lib

import Codec.BMP
import PLY
import Linear.Matrix
import Data.Yaml as Yaml
import Data.Text

data SceneDesc =
    SceneDesc { _sceneDescWorld :: Object
              , _sceneDescCamera :: Object
              , _sceneDescTracer :: String
              }
    deriving (Eq, Show)

instance Yaml.FromJSON SceneDesc where
    parseJSON (Yaml.Object v) =
        SceneDesc <$> v Yaml..: "world"
                    <*> v Yaml..: "camera"
                    <*> v Yaml..: "tracer"
    parseJSON _ = fail "Expected object for SceneDesc"
    

main :: IO ()
main = do result <- Yaml.decodeFileEither "src/cube.yaml"
          case result of 
            Left e -> putStrLn $ show e
            Right (v :: SceneDesc) -> case v of 
                                        (SceneDesc _ n m) -> putStrLn $ show n
                                        _                 -> return () 
          putStrLn "fghjkd"
