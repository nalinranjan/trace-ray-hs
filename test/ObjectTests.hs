{-# OPTIONS -Wall #-}

module ObjectTests where

import           Objects
import           Scene

import           System.IO
import           Test.HUnit
import           Linear.V3
import           Data.Colour.SRGB.Linear
import           Data.Maybe

objectTests :: IO()
objectTests = do hPutStrLn stdout "\n\nObject Tests:\n"
                 hPutStrLn stderr "##########"
                 hPutStrLn stdout "intersectTriTests Tests:"
                 _ <- runTestTT intersectTriTests
                 hPutStrLn stdout "intersectSphereTests Tests:"
                 _ <- runTestTT intersectSphereTests
                 hPutStrLn stdout "normalSphereTest Tests:"
                 _ <- runTestTT normalSphereTest
                 return ()

intersectTriTests :: Test
intersectTriTests = TestList [
  isJust (intersect (Ray {rIoR = 1, rOrigin = V3 0 0 0, rDirection = V3 12 0 0}) 
  (Triangle {tV0 = VertexAttrib {
  vPosition = V3 (-1.4568167e7) (-3010046.3) 1372399.8, vNormal = V3 0.0 1.0 0.0}, tV1 = VertexAttrib {
  vPosition = V3 (-1.2796993e7) (-4578815.0) 1223588.0, vNormal = V3 0.0 1.0 0.0}, tV2 = VertexAttrib {
  vPosition = V3 6659669.0 (-5432374.0) (-291034.38), vNormal = V3 0.0 1.0 0.0}, tMat = MaterialDesc {mIoR = 1, 
  mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0}, tNormal = V3 0.0 1.0 0.0}))
  ~?= False,
  isJust (intersect (Ray {rIoR = 1, rOrigin = V3 0 0 3000, rDirection = V3 0 0 (-1)}) 
  (Triangle {tV0 = VertexAttrib {
  vPosition = V3 100 0 0, vNormal = V3 0.0 1.0 0.0}, tV1 = VertexAttrib {
  vPosition = V3 0 100 100, vNormal = V3 0.0 1.0 0.0}, tV2 = VertexAttrib {
  vPosition = V3 (-100) (-100) 0, vNormal = V3 0.0 1.0 0.0}, tMat = MaterialDesc {mIoR = 1, 
  mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0}, tNormal = V3 0.0 1.0 0.0}))
  ~?= True]

intersectSphereTests :: Test
intersectSphereTests = TestList [
  isJust (intersect (Ray {rIoR = 1, rOrigin = V3 0 200 0, rDirection = V3 0 0 0}) 
  (Sphere {sCenter = V3 0 0 0, 
  sRadius = 500.0, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}))
  ~?= True,
  isJust (intersect (Ray {rIoR = 1, rOrigin = V3 1000 200 0, rDirection = V3 0 0 0}) 
  (Sphere {sCenter = V3 500 0 0, 
  sRadius = 500.0, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}))
  ~?= False]

normalSphereTest :: Test
normalSphereTest = TestList [
  normal (V3 0 0 0) (Sphere {sCenter = V3 0 0 0, 
  sRadius = 500.0, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}) ~?= V3 0 0 0,
  normal (V3 100 200 0) (Sphere {sCenter = V3 0 0 0, 
  sRadius = 500.0, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}) ~?= V3 0.4472136 0.8944272 0.0,
  normal (V3 (-100) 200 (-1000)) (Sphere {sCenter = V3 0 0 (-1000), 
  sRadius = 5030.0, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}) ~?= V3 (-0.4472136) 0.8944272 0,
  normal (V3 (-100) (-200) (-1000)) (Sphere {sCenter = V3 100 50 (-1000), 
  sRadius = 80.0, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}) ~?= V3 (-0.624695) (-0.78086877) 0,
  normal (V3 (-342) (-23424) (-1000)) (Sphere {sCenter = V3 23 134 (-4934), 
  sRadius = 80.0, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}) ~?= V3 (-1.5280276e-2) (-0.9862267) 0.16469206,
  normal (V3 24536 5.637 0.83) (Sphere {sCenter = V3 0.7843 0.1537 0.5623, 
  sRadius = 0.736, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}) ~?= V3 1.0 2.2348695e-4 1.0910846e-5,
  normal (V3 (-2323) (-23234) (-3443)) (Sphere {sCenter = V3 123 12 35453, 
  sRadius = 23233, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}) ~?= V3 (-5.3901523e-2) (-0.5122628) (-0.8571356),
  normal (V3 1.24 2.33 4.55) (Sphere {sCenter = V3 90.87 7438 776, 
  sRadius = 12.234, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}) ~?= V3 (-1.1988841e-2) (-0.99458957) (-0.10318857),
  normal (V3 1 1 1) (Sphere {sCenter = V3 (-10) 10 (-10), 
  sRadius = 35.0, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}) ~?= V3 0.6120564 (-0.50077343) 0.6120564,
  normal (V3 10 10 10) (Sphere {sCenter = V3 100 50 (-1000), 
  sRadius = 10.0, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}}) ~?= V3 (-8.868825e-2) (-3.9417e-2) 0.99527925]