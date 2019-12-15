{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wno-orphans #-}

module Scene where

import           PixelOps()

import           Data.Colour.SRGB.Linear
import           Data.Text               as Text
import           Data.Yaml               as Yaml
import           Linear


{-
  This generates an Orphan instance warning.
-}
instance Yaml.FromJSON (V3 Float) where
  parseJSON (Yaml.String s) = return (read (Text.unpack s))
  parseJSON _               = fail "expected string for V3"

{-
  This generates an Orphan instance warning.
-}
instance Yaml.FromJSON (RGB Float) where
  parseJSON (Yaml.String s) = return (read (Text.unpack s))
  parseJSON _               = fail "expected string for RGB"

{-
  This generates an Orphan instance warning.
-}
instance Yaml.FromJSON (Quaternion Float) where
  parseJSON (Yaml.String s) = return (read (Text.unpack s))
  parseJSON _               = fail "expected string for Quaternion"


data CameraDesc =
  CameraDesc
    { cEyePoint :: V3 Float
    , cLookAt   :: V3 Float
    , cUp       :: V3 Float
    }
  deriving (Eq, Show)

{-
  This generates an Orphan instance warning.
-}
instance Yaml.FromJSON CameraDesc where
  parseJSON (Yaml.Object cd) =
    CameraDesc <$> cd Yaml..: "eyePoint" 
               <*> cd Yaml..: "lookAt" 
               <*> cd Yaml..: "up"
  parseJSON _ = fail "Expected object for CameraDesc"


data ViewPlaneDesc =
  ViewPlaneDesc
    { vWidth    :: Int
    , vHeight   :: Int
    , vDist     :: Float
    , vMaxDepth :: Int
    }
  deriving (Eq, Show)

{-
  This generates an Orphan instance warning.
-}
instance Yaml.FromJSON ViewPlaneDesc where
  parseJSON (Yaml.Object vpd) =
    ViewPlaneDesc <$> vpd Yaml..: "width" 
                  <*> vpd Yaml..: "height" 
                  <*> vpd Yaml..: "dist" 
                  <*> vpd Yaml..: "maxDepth"
  parseJSON _ = fail "Expected object for ViewPlaneDesc"


data LightDesc =
  LightDesc
    { lType     :: String
    , lStrength :: Float
    , lColor    :: RGB Float
    , lPosition :: V3 Float
    }
  deriving (Eq, Show)

{-
  This generates an Orphan instance warning.
-}
instance Yaml.FromJSON LightDesc where
  parseJSON (Yaml.Object ld) =
    LightDesc <$> ld Yaml..: "type" 
              <*> ld Yaml..: "strength" 
              <*> ld Yaml..: "color" 
              <*> ld Yaml..: "position"
  parseJSON _ = fail "Expected object for LightDesc"


data MaterialDesc =
  MaterialDesc
    { mDiffuseColor :: RGB Float
    , mKr           :: Float
    , mKt           :: Float
    , mAlpha        :: Float
    , mIoR          :: Float
    }
  deriving (Eq, Show, Ord)

{-
  This generates an Orphan instance warning.
-}
instance Yaml.FromJSON MaterialDesc where
  parseJSON (Yaml.Object md) =
    MaterialDesc <$> md Yaml..: "diffuseColor" 
                 <*> md Yaml..: "kr"
                 <*> md Yaml..: "kt"
                 <*> md Yaml..: "alpha"
                 <*> md Yaml..: "ior"
  parseJSON _ = fail "Expected object for MaterialDesc"


data TransformDesc =
  TransformDesc
    { tTranslation :: V3 Float
    , tRotation    :: Quaternion Float
    , tScale       :: V3 Float
    }
  deriving (Eq, Show)

{-
  This generates an Orphan instance warning.
-}
instance Yaml.FromJSON TransformDesc where
  parseJSON (Yaml.Object td) =
    TransformDesc <$> td Yaml..: "translation" 
                  <*> td Yaml..: "rotation" 
                  <*> td Yaml..: "scale"
  parseJSON _ = fail "Expected object for TransformDesc"


data ObjectDesc =
  ODMesh FilePath MaterialDesc TransformDesc |
  ODSphere MaterialDesc (V3 Float) Float
  deriving (Eq, Show)

{-
  This generates an Orphan instance warning.
-}
instance Yaml.FromJSON ObjectDesc where
  parseJSON (Yaml.Object od) =
    do typ <- od Yaml..: "type"
       case typ :: String of
         "mesh"   -> ODMesh <$> od Yaml..: "path"
                            <*> od Yaml..: "material"
                            <*> od Yaml..: "transform"
         "sphere" -> ODSphere <$> od Yaml..: "material"
                              <*> od Yaml..: "center"
                              <*> od Yaml..: "radius"
         t        -> fail $ "Unexpected object type: " ++ t
  parseJSON _ = fail "Expected object for ObjectDesc"


data SceneDesc =
  SceneDesc
    { sCamera     :: CameraDesc
    , sViewPlane  :: ViewPlaneDesc
    , sBgColor    :: RGB Float
    , sShadows    :: Bool
    , sOutputFile :: FilePath
    , sAmbient    :: RGB Float
    , sLights     :: [LightDesc]
    , sObjects    :: [ObjectDesc]
    }
  deriving (Eq, Show)

{-
  This generates an Orphan instance warning.
-}
instance Yaml.FromJSON SceneDesc where
  parseJSON (Yaml.Object sd) =
    SceneDesc <$> sd Yaml..: "camera" 
              <*> sd Yaml..: "viewPlane" 
              <*> sd Yaml..: "bgColor" 
              <*> sd Yaml..: "shadows" 
              <*> sd Yaml..: "outputFile" 
              <*> sd Yaml..: "ambient" 
              <*> sd Yaml..: "lights" 
              <*> sd Yaml..: "objects"
  parseJSON _ = fail "Expected object for SceneDesc"
