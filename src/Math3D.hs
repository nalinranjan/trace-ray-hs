module Math3D where

import           Scene

import           Linear
import           Linear.Matrix
import           Linear.Projection


data Ray =
  Ray
    { rOrigin    :: V3 Float
    , rDirection :: V3 Float
    }
  deriving (Eq, Show)

viewMatrix :: CameraDesc -> M44 Float
viewMatrix camera = lookAt e l u
  where
    e = cEyePoint camera
    u = cUp camera
    l = cLookAt camera

projectionMatrix :: ViewPlaneDesc -> M44 Float
projectionMatrix view = perspective 1.57 aspectRatio 10 10000
  where
    w = vWidth view
    h = vHeight view
    aspectRatio = fromIntegral w / fromIntegral h

