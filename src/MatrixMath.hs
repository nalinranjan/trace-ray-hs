{-# LANGUAGE FlexibleContexts   #-}

module MatrixMath where

import           Scene

import           Linear.Matrix
import           Linear.Projection
import           Linear.V3
import           Linear.V4
import           Linear.Metric


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

reflect :: V3 Float -> V3 Float -> V3 Float
reflect v n = normalize $ v - n * vec
  where vDotN = dot v n
        vec   = toV3 $ 2 * vDotN

transmit :: V3 Float -> V3 Float -> Float -> Float -> (V3 Float, Bool)
transmit v n u1 u2 
  | u1 == u2  = (v, False)
  | tir > 1   = (reflect v (-n'), True)
  | otherwise = (normalize (a + b), False)
  where n'      = if dot v n > 0 then n else -n
        u1Divu2 = u1 / u2
        vDotN   = dot v n'
        tir     = u1Divu2 * u1Divu2 * (1 - vDotN * vDotN)
        a       = toV3 u1Divu2 * (v - toV3 vDotN * n')
        b       = toV3 (sqrt (1 - tir)) * n'

toV3 :: Float -> V3 Float
toV3 x = V3 x x x
