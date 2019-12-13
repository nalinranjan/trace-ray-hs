{-# LANGUAGE GADTs #-}

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
  intersect :: Ray -> a -> Maybe (Float, Object)
  material :: a -> MaterialDesc

data Object where
  Object :: (SceneObject a, Ord a) => a -> Object
  -- OTriangle :: Triangle -> Object Triangle
  -- OSphere   :: Sphere   -> Object Sphere

instance Eq Object where
  (Object o1) == (Object o2) = False

instance Ord Object where
  (Object o1) <= (Object o2) = True

instance SceneObject Object where
  intersect r (Object o) = intersect r o
  material  (Object o) = material o


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
    | otherwise = Just (t, Object tri)
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


data Sphere =
  Sphere
    { sCenter :: V3 Float
    , sRadius :: Float
    , sMat    :: MaterialDesc
    }
  deriving (Eq, Show, Ord)
      
instance SceneObject Sphere where
  intersect (Ray o d) sph@(Sphere c r _) 
    | dInt < 0  = Nothing
    | w1 > 0.01 = Just (w1, Object sph)
    | otherwise = Just (w2, Object sph)
    where 
      cToO  = o - c 
      bInt  = dot d cToO * 2
      cInt  = dot cToO cToO - r * r
      dInt  = bInt * bInt - 4 * cInt
      sqrtD = sqrt dInt
      w1    = (-bInt - sqrtD) / 2.0
      w2    = (-bInt + sqrtD) / 2.0
  material = sMat


-- getMaterial :: Object a -> MaterialDesc
-- -- getMaterial (Object o) = eval o
-- --   where eval (Triangle _ _ _ m) = m
-- getMaterial (OTriangle tri) = tMat tri
-- getMaterial (OSphere sph)   = sMat sph
