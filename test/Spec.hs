{-# OPTIONS -Wall #-}

import           SceneTests
import           PixelOpsTests
import           MeshTests
import           MatrixMathTests
import           ObjectTests
import           RayTracerTests

main :: IO ()
main = do sceneTests
          pixelTests
          meshTests
          matrixMathTests
          objectTests
          rayTracerTests