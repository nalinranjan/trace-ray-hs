{-# OPTIONS -Wall #-}

module PixelOpsTests where

import           PixelOps
import           System.IO
import           Test.HUnit
import           Data.Colour.SRGB.Linear

pixelTests :: IO()
pixelTests = do hPutStrLn stdout "\n\nPixelOps Tests:\n"
                hPutStrLn stderr "##########"
                hPutStrLn stdout "floatToWord8 Tests:"
                _ <- runTestTT floatToWord8Test
                hPutStrLn stdout "clamp Tests:"
                _ <- runTestTT clampTests
                hPutStrLn stdout "clampRGB Tests:"
                _ <- runTestTT clampRGBTests
                hPutStrLn stdout "scaleRGB Tests:"
                _ <- runTestTT scaleRGBTests
                hPutStrLn stdout "(!+!) Tests:"
                _ <- runTestTT plusOpTests
                return ()

floatToWord8Test :: Test
floatToWord8Test = 
    TestList [floatToWord8 1 ~?= 255, 
              floatToWord8 0 ~?= 0,
              floatToWord8 0.122 ~?= 31,
              floatToWord8 0.223 ~?= 57,
              floatToWord8 0.3454 ~?= 88,
              floatToWord8 0.432526 ~?= 110,
              floatToWord8 0.858732434 ~?= 219,
              floatToWord8 0.987 ~?= 252,
              floatToWord8 0.324 ~?= 83,
              floatToWord8 0.742 ~?= 189,
              floatToWord8 0.232 ~?= 59]

clampTests :: Test
clampTests = 
  TestList [(clamp 0 0.99 1 :: Float) ~?= (0.99 :: Float),
            (clamp 0 0.5 0 :: Float) ~?= (0 :: Float),
            (clamp 0 0.5 0.122 :: Float) ~?= (0.122 :: Float),
            (clamp 0 0.2 0.223 :: Float) ~?= (0.2 :: Float),
            (clamp 0 0.5 0.3454 :: Float) ~?= (0.3454 :: Float),
            (clamp 0 0 0.432526 :: Float) ~?= (0 :: Float),
            (clamp 0.9 1 0.858732434 :: Float) ~?= (0.9 :: Float),
            (clamp 0.986 0.988 0.987 :: Float) ~?= (0.987 :: Float),
            (clamp 0.324 0.3244 0.324 :: Float) ~?= (0.324 :: Float),
            (clamp 0 1 0.742 :: Float) ~?= (0.742 :: Float),
            (clamp 0.3 0.5 0.232 :: Float) ~?= (0.3 :: Float)]

clampRGBTests :: Test
clampRGBTests = 
  TestList [(clampRGB (RGB (-56) 0.99 1) :: RGB Float) ~?= (RGB 0 0.99 1 :: RGB Float),
            (clampRGB (RGB 0 0.5 67) :: RGB Float) ~?= (RGB 0 0.5 1 :: RGB Float),
            (clampRGB (RGB 0 0.5 122) :: RGB Float) ~?= (RGB 0 0.5 1 :: RGB Float),
            (clampRGB (RGB 0 (-0.2) 0.223) :: RGB Float) ~?= (RGB 0 0 0.223 :: RGB Float),
            (clampRGB (RGB 0 5 0.3454) :: RGB Float) ~?= (RGB 0 1 0.3454 :: RGB Float),
            (clampRGB (RGB 0 0 0.432526) :: RGB Float) ~?= (RGB 0 0 0.432526 :: RGB Float),
            (clampRGB (RGB 0.9 1.7 0.858732434) :: RGB Float) ~?= (RGB 0.9 1 0.858732434 :: RGB Float),
            (clampRGB (RGB 0.986 0.988 0.987) :: RGB Float) ~?= (RGB 0.986 0.988 0.987 :: RGB Float),
            (clampRGB (RGB 324 0.3244 0.324) :: RGB Float) ~?= (RGB 1 0.3244 0.324 :: RGB Float),
            (clampRGB (RGB (-90) 1 (-0.742)) :: RGB Float) ~?= (RGB 0 1 0 :: RGB Float),
            (clampRGB (RGB (-0.3) 0.5 0.232):: RGB Float) ~?= (RGB 0 0.5 0.232 :: RGB Float)]

scaleRGBTests :: Test
scaleRGBTests =
  TestList [scaleRGB 1.0 (RGB 0 0 0) ~?= RGB 0 0 0,
            scaleRGB 1.0 (RGB 0.99 0.98 0.67) ~?= RGB 0.99 0.98 0.67,
            scaleRGB 0.0 (RGB 0.99 0.98 0.67) ~?= RGB 0 0 0,
            scaleRGB 100 (RGB 0.9 0.8 0.7) ~?= RGB 90 80 70,
            scaleRGB 78 (RGB 0.999 0.28 0.17) ~?= RGB 77.922005 21.84 13.26,
            scaleRGB 23 (RGB 0.121 0.9788 0.2267) ~?= RGB 2.783 22.5124 5.2141,
            scaleRGB 0.234 (RGB 0.9789 0.4598 0.6067) ~?= RGB 0.2290626 0.1075932 0.1419678,
            scaleRGB 1.32 (RGB 0.99 0.98 0.67) ~?= RGB 1.3068 1.2936001 0.88440007,
            scaleRGB 89 (RGB 0.2399 0.6898 0.9467) ~?= RGB 21.351099 61.3922 84.256294,
            scaleRGB 1000 (RGB 0.1299 0.2398 0.5667) ~?= RGB 129.9 239.8 566.69995,
            scaleRGB 999 (RGB 0.9369 0.918 0.97) ~?= RGB 935.96313 917.082 969.03]

plusOpTests :: Test
plusOpTests = 
  TestList [(!+!) (RGB 0 0 0) (RGB 0 0 0) ~?= RGB 0 0 0,
            (!+!) (RGB 0.99 0.98 0.67) (RGB 0.99 0.98 0.67) ~?= RGB 1.98 1.96 1.34,
            (!+!) (RGB 0.99 0.98 0.67) (RGB 0 0 0) ~?= RGB 0.99 0.98 0.67,
            (!+!) (RGB 0.9 0.8 0.7) (RGB 90 80 70) ~?= RGB 90.9 80.8 70.7,
            (!+!) (RGB 0.999 0.28 0.17) (RGB 77.922005 21.84 13.26) ~?= RGB 78.921005 22.12 13.43,
            (!+!) (RGB 0.121 0.9788 0.2267) (RGB 2.783 22.5124 5.2141) ~?= RGB 2.904 23.4912 5.4407997,
            (!+!) (RGB 0.9789 0.4598 0.6067) (RGB 0.2290626 0.1075932 0.1419678) ~?= RGB 1.2079626 0.5673932 0.74866784,
            (!+!) (RGB 0.99 0.98 0.67) (RGB 1.3068 1.2936001 0.88440007) ~?= RGB 2.2968001 2.2736 1.5544001,
            (!+!) (RGB 0.2399 0.6898 0.9467) (RGB 21.351099 61.3922 84.256294) ~?= RGB 21.591 62.082 85.202995,
            (!+!) (RGB 0.1299 0.2398 0.5667) (RGB 129.9 239.8 566.69995) ~?= RGB 130.02989 240.03981 567.26666,
            (!+!) (RGB 0.9369 0.918 0.97) (RGB 935.96313 917.082 969.03) ~?= RGB 936.9 918.0 970.0]