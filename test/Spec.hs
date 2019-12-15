import SceneTests
import PixelOpsTests
import MeshTests
import MatrixMathTests
import ObjectTests

import Test.HUnit
import Text.Printf

main :: IO ()
main = do sceneTests
          pixelTests
        --   meshTests
          matrixMathTests
          objectTests