{-# OPTIONS -Wall #-}

module SceneTests where

import           Scene
import           System.IO
import           Test.HUnit
import           Data.Yaml
import           Linear.V3

instance Eq YamlMark where
    (==) = (==) 

instance Eq YamlException where 
    YamlParseException _ _ ym1 == YamlParseException _ _ ym2 = ym1 == ym2
    a                          == b                          = a == b

instance Eq ParseException where
    InvalidYaml    e1 == InvalidYaml e2    = e1 == e2
    AesonException e1 == AesonException e2 = e1 == e2
    _                 == _                 = False

sceneTests :: IO ()
sceneTests = do hPutStrLn stdout "\n\nScene Parser Tests:\n"
                hPutStrLn stderr "##########"
                hPutStrLn stdout "CameraDesc Tests:"
                cameraTP <- cameraTestPositive
                cameraTN <- sequence $ map cameraTestsNegative cameraTestCases
                _ <- runTestTT $ TestList [cameraTP]
                _ <- runTestTT $ TestList cameraTN
                return ()


cameraTestPositive :: IO Test
cameraTestPositive = do result <- decodeFileEither "test/mockFiles/camera1.yaml"
                        return $ result ~?= Right CameraDesc 
                                                    {cEyePoint = V3 0.0 600.0 0.0, 
                                                    cLookAt = V3 0.0 0.0 (-2000.0), 
                                                    cUp = V3 0.0 1.0 0.0}

cameraTestsNegative :: (String, String) -> IO Test
cameraTestsNegative (files, ans) = do result <- decodeFileEither files :: IO (Either ParseException CameraDesc)
                                      return $ result ~?= Left (AesonException ans)

cameraTestCases :: [(String, String)]
cameraTestCases = [("test/mockFiles/camera2.yaml", "Error in $: key \"up\" not found"),
                   ("test/mockFiles/camera3.yaml","Error in $: Expected object for CameraDesc")]
                           


