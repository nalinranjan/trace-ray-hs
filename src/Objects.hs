{-# LANGUAGE GADTs #-}

module Objects where

import           Scene
import           MatrixMath

import           Linear.V3
import           Linear
-- import           Linear.Matrix
-- import           Linear.Metric


data Ray =
  Ray
    { rOrigin    :: V3 Float
    , rDirection :: V3 Float
    , rIoR       :: Float
    }
  deriving (Eq, Show)

epsilon :: Float
-- epsilon = 0.15
epsilon = 0.1

class SceneObject a where
  intersect :: Ray -> a -> Maybe (Float, Object)
  material :: a -> MaterialDesc
  normal :: V3 Float -> a -> V3 Float

data Object where
  Object :: (SceneObject a, Ord a, Show a) => a -> Object
  -- OTriangle :: Triangle -> Object Triangle
  -- OSphere   :: Sphere   -> Object Sphere

instance Show Object where
  show (Object o) = show o

instance Eq Object where
  (Object _) == (Object _) = False

instance Ord Object where
  (Object _) <= (Object _) = True

instance SceneObject Object where
  intersect r (Object o) = intersect r o
  material    (Object o) = material o
  normal    i (Object o) = normal i o


-- instance SceneObject a => SceneObject (Object a) where
--   intersect ray (OTriangle t) = intersect ray t
--   intersect ray (OSphere   s) = intersect ray s


data VertexAttrib =
  VertexAttrib
    { vPosition :: V3 Float
    , vNormal   :: V3 Float
    }
  deriving (Eq, Show, Ord)


data Triangle =
  Triangle
    { tV0     :: VertexAttrib
    , tV1     :: VertexAttrib
    , tV2     :: VertexAttrib
    , tMat    :: MaterialDesc
    , tNormal :: V3 Float
    }
  deriving (Eq, Show, Ord)

instance SceneObject Triangle where
  intersect (Ray o d _) tri@(Triangle v0 v1 v2 _ _)
    | pDote1 == 0                 = Nothing
    | t < epsilon                 = Nothing
    | u < 0 || v < 0 || u + v > 1 = Nothing
    | otherwise                   = Just (t, Object tri)
    where
      v0pos = vPosition v0
      v1pos = vPosition v1
      v2pos = vPosition v2
      e1 = v1pos - v0pos
      e2 = v2pos - v0pos
      p = cross d e2
      pDote1 = dot p e1
      vT = o - v0pos
      q = cross vT e1
      t = dot q e2 / pDote1
      u = dot p vT / pDote1
      v = dot q d / pDote1

  material = tMat
  normal _ = tNormal

data Sphere =
  Sphere
    { sCenter :: V3 Float
    , sRadius :: Float
    , sMat    :: MaterialDesc
    }
  deriving (Eq, Show, Ord)

instance SceneObject Sphere where
  intersect (Ray o d _) sph@(Sphere c r _) 
    | dInt < 0     = Nothing
    | w1 > epsilon = Just (w1, Object sph)
    | w2 > epsilon = Just (w2, Object sph)
    | otherwise    = Nothing
    where 
      cToO  = o - c 
      bInt  = dot d cToO * 2
      cInt  = dot cToO cToO - r * r
      dInt  = bInt * bInt - 4 * cInt
      sqrtD = sqrt dInt
      w1    = (-bInt - sqrtD) / 2.0
      w2    = (-bInt + sqrtD) / 2.0

  material = sMat
  normal pt (Sphere c _ _) = normalize $ pt - c


transformLight :: M44 Float -> LightDesc -> LightDesc
transformLight m (LightDesc t s c p) = LightDesc t s c $ applyTransformPoint m p
