module Objects where

import           Scene

import           Linear.V3
import           Linear


data Ray =
  Ray
    { rOrigin    :: V3 Float
    , rDirection :: V3 Float
    }
  deriving (Eq, Show)


class SceneObject a where
  intersect :: Ray -> a -> Maybe (Float, a)
  

data VertexAttrib =
  VertexAttrib
    { vPosition :: V3 Float
    , vNormal   :: V3 Float
    }
  deriving (Eq, Show, Ord)


data Triangle =
  Triangle
    { tV0  :: VertexAttrib
    , tV1  :: VertexAttrib
    , tV2  :: VertexAttrib
    , tMat :: MaterialDesc
    }
  deriving (Eq, Show, Ord)

instance SceneObject Triangle where
  intersect (Ray o d) tri@(Triangle v0 v1 v2 _)
    | pDote1 == 0 = Nothing
    | t < 0 = Nothing
    | u < 0 || v < 0 || u + v > 1 = Nothing
    | otherwise = Just (t, tri)
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


data Sphere =
  Sphere
    { sCenter :: V3 Float
    , sRadius :: Float
    , sMat    :: MaterialDesc
    }
  deriving (Eq, Show)
      
instance SceneObject Sphere where
  intersect = undefined
