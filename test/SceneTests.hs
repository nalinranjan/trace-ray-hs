{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-orphans #-}

module SceneTests where

import           Scene
import           System.IO
import           Test.HUnit
import           Data.Yaml
import           Linear.V3
import           Data.Colour.SRGB.Linear
import           Linear.Quaternion

{-
  This generates an Orphan instance warning.
-}
instance Eq YamlMark where
    (==) = (==) 

{-
  This generates an Orphan instance warning.
-}
instance Eq YamlException where 
    YamlParseException _ _ ym1 == YamlParseException _ _ ym2 = ym1 == ym2
    a                          == b                          = a == b

{-
  This generates an Orphan instance warning.
-}
instance Eq ParseException where
    InvalidYaml    e1 == InvalidYaml e2    = e1 == e2
    AesonException e1 == AesonException e2 = e1 == e2
    _                 == _                 = False

sceneTests :: IO ()
sceneTests = do hPutStrLn stdout "\n\nScene Parser Tests:\n"
                hPutStrLn stderr "##########"
                hPutStrLn stdout "CameraDesc Tests:"
                cameraTP  <- cameraTestPositive
                cameraTN  <- sequence $ map cameraTestsNegative cameraTestCases
                viewPlane <- sequence $ map viewPlaneDescTests viewPlaneDescTestCase
                light     <- sequence $ map lightDescTests lightDescTestCase
                material  <- sequence $ map materialDescTests materialDescTestCase
                transform <- sequence $ map transformDescTests transformDescTestCase
                obj       <- sequence $ map objectDescTests objectDescTestCase
                scenes    <- sequence $ map sceneDescTests sceneDescTestCase
                _ <- runTestTT $ TestList [cameraTP]
                _ <- runTestTT $ TestList cameraTN
                hPutStrLn stdout "ViewPlaneDesc Tests:"
                _ <- runTestTT $ TestList viewPlane
                hPutStrLn stdout "LightDesc Tests:"
                _ <- runTestTT $ TestList light
                hPutStrLn stdout "MaterialDes Tests:"
                _ <- runTestTT $ TestList material
                hPutStrLn stdout "TransformDesc Tests:"
                _ <- runTestTT $ TestList transform
                hPutStrLn stdout "ObjectDesc Tests:"
                _ <- runTestTT $ TestList obj
                hPutStrLn stdout "SceneDesc Tests:"
                _ <- runTestTT $ TestList scenes
                return ()


cameraTestPositive :: IO Test
cameraTestPositive = do result <- decodeFileEither "test/mockFiles/camera1.yaml"
                        return $ result ~?= Right CameraDesc 
                                                    {cEyePoint = V3 0.0 600.0 0.0, 
                                                    cLookAt = V3 0.0 0.0 (-2000.0), 
                                                    cUp = V3 0.0 1.0 0.0}

cameraTestsNegative :: (FilePath, String) -> IO Test
cameraTestsNegative (files, ans) = do result <- decodeFileEither files :: IO (Either ParseException CameraDesc)
                                      return $ result ~?= Left (AesonException ans)

cameraTestCases :: [(FilePath, String)]
cameraTestCases = [("test/mockFiles/camera2.yaml", "Error in $: key \"up\" not found"),
                   ("test/mockFiles/camera3.yaml","Error in $: Expected object for CameraDesc")]

viewPlaneDescTests :: (FilePath, Either ParseException ViewPlaneDesc) -> IO Test
viewPlaneDescTests (file, ans) = do result <- decodeFileEither file :: IO (Either ParseException ViewPlaneDesc)
                                    return $ result ~?= ans

viewPlaneDescTestCase :: [(FilePath, Either ParseException ViewPlaneDesc)]
viewPlaneDescTestCase = [("test/mockFiles/viewPlane1.yaml", (Right ViewPlaneDesc { vWidth = 640, vHeight = 360, 
                                                              vDist = 200, vMaxDepth = 10})),
                         ("test/mockFiles/viewPlane2.yaml", Left (AesonException "Error in $: key \"dist\" not found")),
                         ("test/mockFiles/viewPlane3.yaml", Left (AesonException "Error in $: Expected object for ViewPlaneDesc"))]

lightDescTests :: (FilePath, Either ParseException LightDesc) -> IO Test
lightDescTests (file, ans) = do result <- decodeFileEither file :: IO (Either ParseException LightDesc)
                                return $ result ~?= ans

lightDescTestCase :: [(FilePath, Either ParseException LightDesc)]
lightDescTestCase = [("test/mockFiles/light1.yaml", (Right LightDesc { lType = "point", lStrength = 1, 
                                                            lColor = RGB {channelRed = 1.0, channelGreen = 1.0, channelBlue = 1.0},
                                                            lPosition = V3 (-500) 1000 (-10)})),
                     ("test/mockFiles/light2.yaml", Left (AesonException "Error in $: key \"color\" not found")),
                     ("test/mockFiles/light3.yaml", Left (AesonException "Error in $: Expected object for LightDesc"))]
                           
materialDescTests :: (FilePath, Either ParseException MaterialDesc) -> IO Test
materialDescTests (file, ans) = do result <- decodeFileEither file :: IO (Either ParseException MaterialDesc)
                                   return $ result ~?= ans

materialDescTestCase :: [(FilePath, Either ParseException MaterialDesc)]
materialDescTestCase = [("test/mockFiles/material1.yaml", (Right MaterialDesc { mDiffuseColor = RGB {channelRed = 0.961, channelGreen = 0.851, 
                                                        channelBlue = 0.651}, mKr = 0, mKt = 0, mAlpha = 20, mIoR = 1})),
                        ("test/mockFiles/material2.yaml", Left (AesonException "Error in $: key \"kr\" not found")),
                        ("test/mockFiles/material3.yaml", Left (AesonException "Error in $: Expected object for MaterialDesc"))]
                                                
transformDescTests :: (FilePath, Either ParseException TransformDesc) -> IO Test
transformDescTests (file, ans) = do result <- decodeFileEither file :: IO (Either ParseException TransformDesc)
                                    return $ result ~?= ans

transformDescTestCase :: [(FilePath, Either ParseException TransformDesc)]
transformDescTestCase = [("test/mockFiles/transform1.yaml", (Right TransformDesc { tTranslation = V3 0 100 (-1500),
                                                              tRotation = Quaternion 1 (V3 0 0 0),
                                                              tScale = V3 75 75 75})),
                         ("test/mockFiles/transform2.yaml", Left (AesonException "Error in $: key \"translation\" not found")),
                         ("test/mockFiles/transform3.yaml", Left (AesonException "Error in $: Expected object for TransformDesc"))]
                      
objectDescTests :: (FilePath, Either ParseException ObjectDesc) -> IO Test
objectDescTests (file, ans) = do result <- decodeFileEither file :: IO (Either ParseException ObjectDesc)
                                 return $ result ~?= ans

objectDescTestCase :: [(FilePath, Either ParseException ObjectDesc)]
objectDescTestCase = [
  ("test/mockFiles/object1.yaml", (Right (ODMesh "meshes/wall.ply" (MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 0.961, channelGreen = 0.851, 
  channelBlue = 0.651}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0}) (TransformDesc {tTranslation = V3 0.0 0.0 (-200.0), 
  tRotation = Quaternion 0.525 (V3 0.0 (-0.851) 0.0), tScale = V3 350.0 350.0 350.0})))),
  ("test/mockFiles/object2.yaml", Left (AesonException "Error in $: key \"path\" not found")),
  ("test/mockFiles/object3.yaml", Left (AesonException "Error in $: Expected object for ObjectDesc"))]

sceneDescTests :: (FilePath, Either ParseException SceneDesc) -> IO Test
sceneDescTests (file, ans) = do result <- decodeFileEither file
                                return $ result ~?= ans
  
sceneDescTestCase :: [(FilePath, Either ParseException SceneDesc)]
sceneDescTestCase = [
  ("test/mockFiles/scene1.yaml", (Right (SceneDesc {sCamera = CameraDesc {cEyePoint = V3 0.0 600.0 0.0, 
  cLookAt = V3 0.0 0.0 (-2000.0), cUp = V3 0.0 1.0 0.0}, sViewPlane = ViewPlaneDesc {vWidth = 1200, vHeight = 1200, 
  vDist = 500.0, vMaxDepth = 10}, sBgColor = RGB {channelRed = 0.0, channelGreen = 0.0, channelBlue = 0.0}, 
  sShadows = True, sOutputFile = "renders/wall.bmp", sAmbient = RGB {channelRed = 0.1, channelGreen = 0.1, 
  channelBlue = 0.1}, sLights = [LightDesc {lType = "point", lStrength = 1.0, lColor = RGB {channelRed = 0.8, 
  channelGreen = 0.8, channelBlue = 0.8}, lPosition = V3 600.0 800.0 (-800.0)},LightDesc {lType = "point", 
  lStrength = 1.0, lColor = RGB {channelRed = 0.8, channelGreen = 0.8, channelBlue = 0.8},
  lPosition = V3 (-600.0) 800.0 (-2000.0)}], sObjects = [ODMesh "meshes/plane_back.ply" (MaterialDesc {mDiffuseColor = 
  RGB {channelRed = 0.961, channelGreen = 0.851, channelBlue = 0.651}, mKr = 0.0, mKt = 0.0, mAlpha = 200.0, 
  mIoR = 1.0}) (TransformDesc {tTranslation = V3 0.0 (-1000.0) (-1500.0), tRotation = Quaternion 1.0 (V3 0.0 0.0 0.0),
  tScale = V3 2000.0 2000.0 2000.0}),ODMesh "meshes/plane_bottom.ply" (MaterialDesc {mDiffuseColor = 
  RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 200.0, mIoR = 1.0}) 
  (TransformDesc {tTranslation = V3 0.0 (-1000.0) (-1500.0), tRotation = Quaternion 0.995 (V3 0.0 9.8e-2 0.0), 
  tScale = V3 1000.0 1000.0 1000.0}),ODSphere (MaterialDesc {mDiffuseColor = RGB {channelRed = 1.0, 
  channelGreen = 0.843, channelBlue = 0.0}, mKr = 0.0, mKt = 0.0, mAlpha = 200.0, mIoR = 1.0}) 
  (V3 0.0 200.0 (-1000.0)) 500.0]}))),
  ("test/mockFiles/scene2.yaml", Left (AesonException "Error in $: key \"bgColor\" not found")),
  ("test/mockFiles/scene3.yaml", Left (AesonException "Error in $: Expected object for SceneDesc"))]


                         