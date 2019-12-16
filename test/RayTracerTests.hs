module RayTracerTests where

import           RayTracer
import           Objects
import           Scene
import           PixelOps

import           Linear.V3
import           System.IO
import           Test.HUnit

rayTracerTests :: IO()
rayTracerTests = do hPutStrLn stdout "\nRayTracer Tests:\n"
                    hPutStrLn stderr "##########"
                    hPutStrLn stdout "toV3 Tests:"
                    _ <- runTestTT toV3Tests
                    hPutStrLn stdout "reflect Tests:"
                    _ <- runTestTT reflectTests
                    return ()

toV3Tests :: Test
toV3Tests = TestList [
  toV3 1 ~?= V3 1 1 1,
  toV3 0 ~?= V3 0 0 0,
  toV3 9.8 ~?= V3 9.8 9.8 9.8,
  toV3 5.34 ~?= V3 5.34 5.34 5.34,
  toV3 0.567 ~?= V3 0.567 0.567 0.567,
  toV3 8.679 ~?= V3 8.679 8.679 8.679,
  toV3 (-9.43) ~?= V3 (-9.43) (-9.43) (-9.43),
  toV3 (-1.23476) ~?= V3 (-1.23476) (-1.23476) (-1.23476),
  toV3 (-7.536) ~?= V3 (-7.536) (-7.536) (-7.536),
  toV3 0.68127235 ~?= V3 0.68127235 0.68127235 0.68127235]

reflectTests :: Test
reflectTests = TestList [
  reflect (V3 0 0 0) (V3 0 0 0) ~?= V3 0 0 0,
  reflect (V3 0.99 0.98 0.67) (V3 0.99 0.98 0.67) ~?= V3 (-0.6404581) (-0.6339889) (-0.43344137),
  reflect (V3 0.99 0.98 0.67) (V3 0 0 0) ~?= V3 0.64045817 0.6339889 0.4334414,
  reflect (V3 0.9 0.8 0.7) (V3 90 80 70) ~?= V3 (-0.6461624) (-0.5743665) (-0.50257075),
  reflect (V3 0.999 0.28 0.17) (V3 77.922005 21.84 13.26) ~?= V3 (-0.95022225) (-0.26632854) (-0.16169947),
  reflect (V3 0.121 0.9788 0.2267) (V3 2.783 22.5124 5.2141) ~?= V3 (-0.11956876) (-0.96722233) (-0.22401848),
  reflect (V3 0.9789 0.4598 0.6067) (V3 0.2290626 0.1075932 0.1419678) ~?= V3 0.789398 0.37078884 0.48925096,
  reflect (V3 0.99 0.98 0.67) (V3 1.3068 1.2936001 0.88440007) ~?= V3 (-0.6404581) (-0.63398886) (-0.4334414),
  reflect (V3 0.2399 0.6898 0.9467) (V3 21.351099 61.3922 84.256294) ~?= V3 (-0.20064133) (-0.57691705) (-0.79177636),
  reflect (V3 0.1299 0.2398 0.5667) (V3 129.9 239.8 566.69995) ~?= V3 (-0.20654806) (-0.38129508) (-0.90108377),
  reflect (V3 0.9369 0.918 0.97) (V3 935.96313 917.082 969.03) ~?= V3 (-0.57429856) (-0.56271327) (-0.59458816)]