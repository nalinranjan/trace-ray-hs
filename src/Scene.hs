{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scene where

import           Data.Colour
import           Data.Colour.SRGB.Linear
import           Data.Text               as Text
import           Data.Yaml               as Yaml
import           Linear
import           Linear.Quaternion

instance Yaml.FromJSON (V3 Float) where
  parseJSON (Yaml.String s) = return (read (Text.unpack s))
  parseJSON _               = fail "expected string V3"

instance Yaml.FromJSON (RGB Float) where
  parseJSON (Yaml.String s) = return (read (Text.unpack s))
  parseJSON _               = fail "expected string RGB"

instance Yaml.FromJSON (Quaternion Float) where
  parseJSON (Yaml.String s) = return (read (Text.unpack s))
  parseJSON _               = fail "expected string Quaternion"

data CameraDesc =
  CameraDesc
    { cEyePoint :: V3 Float
    , cLookAt   :: V3 Float
    , cUp       :: V3 Float
    }
  deriving (Eq, Show)

instance Yaml.FromJSON CameraDesc where
  parseJSON (Yaml.Object cd) =
    CameraDesc <$> cd Yaml..: "eyePoint" <*> cd Yaml..: "lookAt" <*>
    cd Yaml..: "up"
  parseJSON _ = fail "Expected object for CameraDesc"

data ViewPlaneDesc =
  ViewPlaneDesc
    { vWidth    :: Int
    , vHeight   :: Int
    , vDist     :: Int
    , vMaxDepth :: Int
    }
  deriving (Eq, Show)

instance Yaml.FromJSON ViewPlaneDesc where
  parseJSON (Yaml.Object vpd) =
    ViewPlaneDesc <$> vpd Yaml..: "width" <*> vpd Yaml..: "height" <*>
    vpd Yaml..: "dist" <*>
    vpd Yaml..: "maxDepth"
  parseJSON _ = fail "Expected object for ViewPlaneDesc"

data AmbientDesc =
  AmbientDesc
    { aColor    :: RGB Float
    , aStrength :: Float
    }
  deriving (Eq, Show)

instance Yaml.FromJSON AmbientDesc where
  parseJSON (Yaml.Object ad) =
    AmbientDesc <$> ad Yaml..: "color" <*> ad Yaml..: "strength"
  parseJSON _ = fail "Expected object for AmbientDesc"

data LightDesc =
  LightDesc
    { lType     :: String
    , lStrength :: Float
    , lColor    :: RGB Float
    , lPosition :: V3 Float
    }
  deriving (Eq, Show)

instance Yaml.FromJSON LightDesc where
  parseJSON (Yaml.Object ld) =
    LightDesc <$> ld Yaml..: "type" <*> ld Yaml..: "strength" <*>
    ld Yaml..: "color" <*>
    ld Yaml..: "position"
  parseJSON _ = fail "Expected object for LightDesc"

data MaterialDesc =
  MaterialDesc
    { mType         :: String
    , mDiffuseColor :: RGB Float
    }
  deriving (Eq, Show)

instance Yaml.FromJSON MaterialDesc where
  parseJSON (Yaml.Object md) =
    MaterialDesc <$> md Yaml..: "type" <*> md Yaml..: "diffuseColor"
  parseJSON _ = fail "Expected object for MaterialDesc"

data TransformDesc =
  TransformDesc
    { tTranslation :: V3 Float
    , tRotation    :: Quaternion Float
    , tScale       :: V3 Float
    }
  deriving (Eq, Show)

instance Yaml.FromJSON TransformDesc where
  parseJSON (Yaml.Object td) =
    TransformDesc <$> td Yaml..: "translation" <*> td Yaml..: "rotation" <*>
    td Yaml..: "scale"
  parseJSON _ = fail "Expected object for TransformDesc"

data ObjectDesc =
  ObjectDesc
    { oType      :: String
    , oPath      :: FilePath
    , oMaterial  :: MaterialDesc
    , oTransform :: TransformDesc
    }
  deriving (Eq, Show)

instance Yaml.FromJSON ObjectDesc where
  parseJSON (Yaml.Object od) =
    ObjectDesc <$> od Yaml..: "type" <*> od Yaml..: "path" <*>
    od Yaml..: "material" <*>
    od Yaml..: "transform"
  parseJSON _ = fail "Expected object for ObjectDesc"

data SceneDesc =
  SceneDesc
    { sCamera    :: CameraDesc
    , sViewPlane :: ViewPlaneDesc
    , sBgColor   :: RGB Float
    , sShadows   :: Bool
    , sAmbient   :: AmbientDesc
    , sLights    :: [LightDesc]
    , sObjects   :: [ObjectDesc]
    }
  deriving (Eq, Show)

instance Yaml.FromJSON SceneDesc where
  parseJSON (Yaml.Object sd) =
    SceneDesc <$> sd Yaml..: "camera" <*> sd Yaml..: "viewPlane" <*>
    sd Yaml..: "bgColor" <*>
    sd Yaml..: "shadows" <*>
    sd Yaml..: "ambient" <*>
    sd Yaml..: "lights" <*>
    sd Yaml..: "objects"
  parseJSON _ = fail "Expected object for SceneDesc"