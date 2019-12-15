{-# LANGUAGE FlexibleContexts   #-}

module MatrixMath where

import           Scene

import           Linear.Matrix
import           Linear.Projection
import           Linear.V3
import           Linear.V4


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
  where
    r = tRotation tf
    t = tTranslation tf
    (V3 sx sy sz) = tScale tf
    scalingMatrix = V4 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0) (V4 0 0 0 1)


applyTransformPoint :: M44 Float -> V3 Float -> V3 Float
applyTransformPoint matrix = normalizePoint . (matrix !*) . point

transformLight :: M44 Float -> LightDesc -> LightDesc
transformLight m (LightDesc t s c p) = LightDesc t s c $ applyTransformPoint m p
