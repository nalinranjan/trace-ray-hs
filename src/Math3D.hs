module Math3D where

import           Scene

import           Linear
import           Linear.V4
import           Linear.Matrix
import           Linear.Projection
-- import           Data.Vector.Storable (Vector)
import           Data.Vector as Vec


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
    l = cLookAt camera
    u = cUp camera

projectionMatrix :: ViewPlaneDesc -> M44 Float
projectionMatrix view = perspective 1.57 aspectRatio 10 10000
  where
    w = vWidth view
    h = vHeight view
    aspectRatio = fromIntegral w / fromIntegral h

worldMatrix :: TransformDesc -> M44 Float
worldMatrix tf = mkTransformation r t !*! scalingMatrix
  where r = tRotation tf
        t = tTranslation tf
        (V3 sx sy sz) = tScale tf
        scalingMatrix = V4 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0) (V4 0 0 0 1)
