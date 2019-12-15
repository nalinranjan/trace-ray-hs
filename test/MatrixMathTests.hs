{-# OPTIONS -Wall #-}

module MatrixMathTests where

import           MatrixMath
import           Scene

import           Linear.V3
import           Linear.V4
import           System.IO
import           Test.HUnit
import           Linear.Quaternion

matrixMathTests :: IO()
matrixMathTests = do hPutStrLn stdout "\nMatrixMath Tests:\n"
                     hPutStrLn stderr "##########"
                     hPutStrLn stdout "viewMatrix Tests:"
                     _ <- runTestTT viewMatrixTests
                     hPutStrLn stdout "projectionMatrix Tests:"
                     _ <- runTestTT projectMatrixTests
                     hPutStrLn stdout "worldMatrix Tests:"
                     _ <- runTestTT worldMatrixTests
                     hPutStrLn stdout "applyTransformPoint Tests:"
                     _ <- runTestTT applyTransformPointTests
                     hPutStrLn stdout "toV3Tests Tests:"
                     _ <- runTestTT toV3Tests
                     return ()

viewMatrixTests :: Test
viewMatrixTests = TestList [
  viewMatrix (CameraDesc {cEyePoint = V3 0.0 600.0 0.0, cLookAt = V3 0.0 0.0 (-2000.0), cUp = V3 0.0 1.0 0.0}) 
  ~?= V4 (V4 1.0 (-0.0) 0.0 (-0.0)) (V4 0.0 0.9578263 (-0.28734788) (-574.6958)) (V4 (-0.0) 0.28734788 0.9578263 (-172.40874)) 
  (V4 0.0 0.0 0.0 1.0),
  viewMatrix (CameraDesc {cEyePoint = V3 0.0 0.0 0.0, cLookAt = V3 3 0.0 (-2000.0), cUp = V3 90.0 1.0 0.0}) 
  ~?= V4 (V4 1.1110426e-2 (-0.9999383) 1.666564e-5 (-0.0)) (V4 0.99993724 1.1110439e-2 1.4999058e-3 (-0.0)) 
  (V4 (-1.4999984e-3) (-0.0) 0.9999989 0.0) (V4 0.0 0.0 0.0 1.0),
  viewMatrix (CameraDesc {cEyePoint = V3 0 0 (-1), cLookAt = V3 1 2 3, cUp = V3 3 4 5}) 
  ~?= V4 (V4 (-0.63599867) 0.7419985 (-0.21199958) (-0.21199958)) (V4 0.7401936 0.50888306 (-0.4394899) (-0.4394899)) 
  (V4 (-0.21821788) (-0.43643576) (-0.8728715) (-0.8728715)) (V4 0.0 0.0 0.0 1.0),
  viewMatrix (CameraDesc {cEyePoint = V3 11 17389 45, cLookAt = V3 12 23 34, cUp = V3 1 1 1}) 
  ~?= V4 (V4 (-0.7068623) (-4.8875523e-4) 0.707351 (-15.556349)) (V4 0.70735115 (-4.070098e-4) 0.7068621 (-32.512165)) 
  (V4 (-5.7583773e-5) 0.99999976 6.3342147e-4 (-17389.025)) (V4 0.0 0.0 0.0 1.0),
  viewMatrix (CameraDesc {cEyePoint = V3 0 0 0, cLookAt = V3 0 0 0, cUp = V3 0 0 0}) 
  ~?= V4 (V4 0.0 0.0 0.0 (-0.0)) (V4 0.0 0.0 0.0 (-0.0)) (V4 (-0.0) (-0.0) (-0.0) 0.0) (V4 0.0 0.0 0.0 1.0),
  viewMatrix (CameraDesc {cEyePoint = V3 0.67 0.624 0.56372, cLookAt = V3 500 100 500, cUp = V3 1000 10000 167}) 
  ~?= V4 (V4 (-0.7118236) 5.949512e-2 0.6998339 4.5286477e-2) (V4 (-5.5851277e-2) 0.9884558 (-0.14083992) (-0.49998182)) 
  (V4 (-0.7001342) (-0.13933979) (-0.7002832) 0.9508016) (V4 0.0 0.0 0.0 1.0),
  viewMatrix (CameraDesc {cEyePoint = V3 56 45 1, cLookAt = V3 0 0 0, cUp = V3 0 1 2}) 
  ~?= V4 (V4 (-0.57932836) 0.7290424 (-0.3645212) 1.7881393e-7) (V4 (-0.23845786) 0.27605668 0.93109107 (-1.66893e-6)) 
  (V4 0.7794334 0.62633044 1.3918454e-2 (-71.84706)) (V4 0.0 0.0 0.0 1.0),
  viewMatrix (CameraDesc {cEyePoint = V3 1 1 1, cLookAt = V3 1 1 1, cUp = V3 1 1 1}) 
  ~?= V4 (V4 0.0 0.0 0.0 (-0.0)) (V4 0.0 0.0 0.0 (-0.0)) (V4 (-0.0) (-0.0) (-0.0) 0.0) (V4 0.0 0.0 0.0 1.0),
  viewMatrix (CameraDesc {cEyePoint = V3 23 32 13, cLookAt = V3 56 54 56, cUp = V3 78 89 76}) 
  ~?= V4 (V4 (-0.82334816) 0.32322627 0.46650034 2.5292625) (V4 6.2151358e-2 0.86838186 (-0.49198595) (-22.821884)) 
  (V4 (-0.56412315) (-0.37608212) (-0.7350696) 34.565365) (V4 0.0 0.0 0.0 1.0),
  viewMatrix (CameraDesc {cEyePoint = V3 23 21 34, cLookAt = V3 9 9 9, cUp = V3 (-1) (-88) (-98)}) 
  ~?= V4 (V4 (-0.4908923) (-0.64573437) 0.58485216 4.965971) (V4 0.7455978 (-0.65863836) (-0.10138835) 0.12986112) 
  (V4 0.45067593 0.38629368 0.80477846 (-45.84018)) (V4 0.0 0.0 0.0 1.0)]
  
projectMatrixTests :: Test
projectMatrixTests = TestList [
  projectionMatrix (ViewPlaneDesc {vWidth = 600, vHeight = 400, vDist = 500.0, vMaxDepth = 10})
  ~?= V4 (V4 0.66719776 0.0 0.0 0.0) (V4 0.0 1.0007966 0.0 0.0) (V4 0.0 0.0 (-1.002002) (-20.02002)) (V4 0.0 0.0 (-1.0) 0.0),
  projectionMatrix (ViewPlaneDesc {vWidth = 1, vHeight = 0, vDist = 0, vMaxDepth = 1320})
  ~?= V4 (V4 0.0 0.0 0.0 0.0) (V4 0.0 1.0007966 0.0 0.0) (V4 0.0 0.0 (-1.002002) (-20.02002)) (V4 0.0 0.0 (-1.0) 0.0),
  projectionMatrix (ViewPlaneDesc {vWidth = 1, vHeight = 1, vDist = 1, vMaxDepth = 150})
  ~?= V4 (V4 1.0007966 0.0 0.0 0.0) (V4 0.0 1.0007966 0.0 0.0) (V4 0.0 0.0 (-1.002002) (-20.02002)) (V4 0.0 0.0 (-1.0) 0.0),
  projectionMatrix (ViewPlaneDesc {vWidth = 12, vHeight = 56, vDist = 32.0, vMaxDepth = 0})
  ~?= V4 (V4 4.670384 0.0 0.0 0.0) (V4 0.0 1.0007966 0.0 0.0) (V4 0.0 0.0 (-1.002002) (-20.02002)) (V4 0.0 0.0 (-1.0) 0.0),
  projectionMatrix (ViewPlaneDesc {vWidth = 45, vHeight = 1, vDist = 300, vMaxDepth = 10})
  ~?= V4 (V4 2.2239925e-2 0.0 0.0 0.0) (V4 0.0 1.0007966 0.0 0.0) (V4 0.0 0.0 (-1.002002) (-20.02002)) (V4 0.0 0.0 (-1.0) 0.0),
  projectionMatrix (ViewPlaneDesc {vWidth = 60120, vHeight = 40420, vDist = 52300, vMaxDepth = 100})
  ~?= V4 (V4 0.6728576 0.0 0.0 0.0) (V4 0.0 1.0007966 0.0 0.0) (V4 0.0 0.0 (-1.002002) (-20.02002)) (V4 0.0 0.0 (-1.0) 0.0),
  projectionMatrix (ViewPlaneDesc {vWidth = 8700, vHeight = 100, vDist = 500.67, vMaxDepth = 1670})
  ~?= V4 (V4 1.150341e-2 0.0 0.0 0.0) (V4 0.0 1.0007966 0.0 0.0) (V4 0.0 0.0 (-1.002002) (-20.02002)) (V4 0.0 0.0 (-1.0) 0.0),
  projectionMatrix (ViewPlaneDesc {vWidth = 600, vHeight = 400, vDist = 0.89, vMaxDepth = 90})
  ~?= V4 (V4 0.66719776 0.0 0.0 0.0) (V4 0.0 1.0007966 0.0 0.0) (V4 0.0 0.0 (-1.002002) (-20.02002)) (V4 0.0 0.0 (-1.0) 0.0),
  projectionMatrix (ViewPlaneDesc {vWidth = 123, vHeight = 3474, vDist = 1.11, vMaxDepth = 190})
  ~?= V4 (V4 28.266403 0.0 0.0 0.0) (V4 0.0 1.0007966 0.0 0.0) (V4 0.0 0.0 (-1.002002) (-20.02002)) (V4 0.0 0.0 (-1.0) 0.0),
  projectionMatrix (ViewPlaneDesc {vWidth = 77777, vHeight = 34, vDist = 5.67498586345, vMaxDepth = 18})
  ~?= V4 (V4 4.3749544e-4 0.0 0.0 0.0) (V4 0.0 1.0007966 0.0 0.0) (V4 0.0 0.0 (-1.002002) (-20.02002)) (V4 0.0 0.0 (-1.0) 0.0)]

worldMatrixTests :: Test
worldMatrixTests = TestList [
  worldMatrix (TransformDesc {tTranslation = V3 0.0 (-1300.0) (-2000.0), tRotation = Quaternion 1.0 (V3 0.0 0.0 0.0), tScale = V3 350.0 350.0 350.0})
  ~?= V4 (V4 350.0 0.0 0.0 0.0) (V4 0.0 350.0 0.0 (-1300.0)) (V4 0.0 0.0 350.0 (-2000.0)) (V4 0.0 0.0 0.0 1.0),
  worldMatrix (TransformDesc {tTranslation = V3 0 0 0, tRotation = Quaternion 1.0 (V3 0.0 0.0 0.0), tScale = V3 100.0 200.0 788.0})
  ~?= V4 (V4 100.0 0.0 0.0 0.0) (V4 0.0 200.0 0.0 0.0) (V4 0.0 0.0 788.0 0.0) (V4 0.0 0.0 0.0 1.0),
  worldMatrix (TransformDesc {tTranslation = V3 1 1 1, tRotation = Quaternion 1.0 (V3 0.0 0.0 0.0), tScale = V3 1 1 1})
  ~?= V4 (V4 1.0 0.0 0.0 1.0) (V4 0.0 1.0 0.0 1.0) (V4 0.0 0.0 1.0 1.0) (V4 0.0 0.0 0.0 1.0),
  worldMatrix (TransformDesc {tTranslation = V3 0.98 0.3773 0.78, tRotation = Quaternion 0.87 (V3 0.76 0.89 0.23), tScale = V3 0.638 0.765 0.2})
  ~?= V4 (V4 (-0.44021997) 0.72873896 0.37963998 0.98) (V4 1.118414 (-0.19966502) (-0.1826) 0.3773) (V4 (-0.764962) 1.324827 (-0.34787998) 0.78) 
  (V4 0.0 0.0 0.0 1.0),
  worldMatrix (TransformDesc {tTranslation = V3 650 200 (-2000), tRotation = Quaternion 1.09 (V3 0.78 0.67 8.90), tScale = V3 100 100 100})
  ~?= V4 (V4 (-15831.778) (-1835.68) 1534.4598 650.0) (V4 2044.7201 (-15863.678) 1022.55994 200.0) (V4 1242.3398 1362.64 (-111.45999) (-2000.0)) 
  (V4 0.0 0.0 0.0 1.0),
  worldMatrix (TransformDesc {tTranslation = V3 (-700) 200 (-2500), tRotation = Quaternion 13.0 (V3 120 45 40.0), tScale = V3 78 645 782})
  ~?= V4 (V4 (-565422.0) 6295200.0 8422140.0 (-700.0)) (V4 923520.0 (-2.0639356e7) 375360.0 200.0) (V4 657540.0 4334400.0 (-2.5687918e7) (-2500.0)) 
  (V4 0.0 0.0 0.0 1.0),
  worldMatrix (TransformDesc {tTranslation = V3 0 (-300) (-9900), tRotation = Quaternion 0.111 (V3 1.23 1.1 1.89), tScale = V3 0 0 0})
  ~?= V4 (V4 0.0 0.0 0.0 0.0) (V4 0.0 0.0 0.0 (-300.0)) (V4 0.0 0.0 0.0 (-9900.0)) (V4 0.0 0.0 0.0 1.0),
  worldMatrix (TransformDesc {tTranslation = V3 0 0 (-700), tRotation = Quaternion 0.493 (V3 0.366 0.12 0.34), tScale = V3 98 974 7})
  ~?= V4 (V4 72.520004 (-240.96762) 2.5704 0.0) (V4 41.46184 487.8649 (-1.9549321) 0.0) (V4 12.794881 430.9716 4.9230156 (-700.0)) 
  (V4 0.0 0.0 0.0 1.0),
  worldMatrix (TransformDesc {tTranslation = V3 0 (-300) (-500), tRotation = Quaternion 0.93 (V3 0.366 0 0), tScale = V3 23 24 1})
  ~?= V4 (V4 23.0 0.0 0.0 0.0) (V4 0.0 17.57011 (-0.68076) (-300.0)) (V4 0.0 16.338242 0.73208797 (-500.0)) (V4 0.0 0.0 0.0 1.0),
  worldMatrix (TransformDesc {tTranslation = V3 600 0 (-500), tRotation = Quaternion 0.995 (V3 0 0.098 0), tScale = V3 11 1 1})
  ~?= V4 (V4 10.788712 0.0 0.19501999 600.0) (V4 0.0 1.0 0.0 0.0) (V4 (-2.1452198) 0.0 0.980792 (-500.0)) (V4 0.0 0.0 0.0 1.0)]

applyTransformPointTests :: Test
applyTransformPointTests = TestList [
  applyTransformPoint 
  (V4 (V4 10.788712 0.0 0.19501999 600.0) (V4 0.0 1.0 0.0 0.0) (V4 (-2.1452198) 0.0 0.980792 (-500.0)) (V4 0.0 0.0 0.0 1.0))
  (V3 17.57011 (-0.68076) (-300.0)) 
  ~?= V3 731.05286 (-0.68076) (-831.9293),
  applyTransformPoint 
  (V4 (V4 23.0 0.0 0.0 0.0) (V4 0.0 17.57011 (-0.68076) (-300.0)) (V4 0.0 16.338242 0.73208797 (-500.0)) (V4 0.0 0.0 0.0 1.0))
  (V3 (-0.764962) 1.324827 (-0.34787998)) 
  ~?= V3 (-17.594126) (-276.4858) (-478.60934),
  applyTransformPoint 
  (V4 (V4 72.520004 (-240.96762) 2.5704 0.0) (V4 41.46184 487.8649 (-1.9549321) 0.0) (V4 12.794881 430.9716 4.9230156 (-700.0)) 
  (V4 0.0 0.0 0.0 1.0))
  (V3 1.118414 (-0.19966502) (-0.1826)) 
  ~?= V3 128.75084 (-50.681076) (-772.6389),
  applyTransformPoint 
  (V4 (V4 0.0 0.0 0.0 0.0) (V4 0.0 0.0 0.0 (-300.0)) (V4 0.0 0.0 0.0 (-9900.0)) (V4 0.0 0.0 0.0 1.0))
  (V3 (-0.44021997) 0.72873896 0.37963998) 
  ~?= V3 0.0 (-300.0) (-9900.0),
  applyTransformPoint 
  (V4 (V4 (-565422.0) 6295200.0 8422140.0 (-700.0)) (V4 923520.0 (-2.0639356e7) 375360.0 200.0) (V4 657540.0 4334400.0 (-2.5687918e7) (-2500.0)) 
  (V4 0.0 0.0 0.0 1.0))
  (V3 12.794881 430.9716 4.9230156) 
  ~?= V3 2.7472794e9 (-8.881312e9) 1.7499517e9,
  applyTransformPoint 
  (V4 (V4 (-15831.778) (-1835.68) 1534.4598 650.0) (V4 2044.7201 (-15863.678) 1022.55994 200.0) (V4 1242.3398 1362.64 (-111.45999) (-2000.0)) 
  (V4 0.0 0.0 0.0 1.0))
  (V3 41.46184 487.8649 (-1.9549321))
  ~?= V3 (-1554328.3) (-7656352.5) 714511.8,
  applyTransformPoint 
  (V4 (V4 (-0.44021997) 0.72873896 0.37963998 0.98) (V4 1.118414 (-0.19966502) (-0.1826) 0.3773) (V4 (-0.764962) 1.324827 (-0.34787998) 0.78) 
  (V4 0.0 0.0 0.0 1.0))
  (V3 72.520004 (-240.96762) 2.5704)
  ~?= V3 (-205.57143) 129.12814 (-374.82962),
  applyTransformPoint 
  (V4 (V4 1.0 0.0 0.0 1.0) (V4 0.0 1.0 0.0 1.0) (V4 0.0 0.0 1.0 1.0) (V4 0.0 0.0 0.0 1.0))
  (V3 0.0 (-1.002002) (-20.02002))
  ~?= V3 1.0 (-2.0020008e-3) (-19.02002),
  applyTransformPoint 
  (V4 (V4 100.0 0.0 0.0 0.0) (V4 0.0 200.0 0.0 0.0) (V4 0.0 0.0 788.0 0.0) (V4 0.0 0.0 0.0 1.0))
  (V3 0.0 1.0007966 0.0) 
  ~?= V3 0.0 200.15932 0.0,
  applyTransformPoint 
  (V4 (V4 350.0 0.0 0.0 0.0) (V4 0.0 350.0 0.0 (-1300.0)) (V4 0.0 0.0 350.0 (-2000.0)) (V4 0.0 0.0 0.0 1.0))
  (V3 28.266403 0.0 0.0) 
  ~?= V3 9893.241 (-1300.0) (-2000.0)]

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
