{-# LANGUAGE OverloadedStrings   #-}

module Mesh where

import           Scene
import           Math3D

import           Linear
import           Linear.Matrix
import           Data.Vector as Vec
import           PLY
import           PLY.Types

data VertexAttrib = 
  VertexAttrib
    { vPosition :: V3 Float
    , vNormal   :: V3 Float
    }
  deriving (Eq, Show)

data Triangle = 
  Triangle
    { tV0  :: VertexAttrib
    , tV1  :: VertexAttrib
    , tV2  :: VertexAttrib
    , tMat :: MaterialDesc
    }
  deriving (Eq, Show)

data Mesh = 
  Mesh
    { mVertices :: Vec.Vector VertexAttrib
    -- , mIndices  :: Vec.Vector (V3 Int)
    , mIndices  :: [V3 Int]
    }
  deriving (Eq, Show)

data Sphere = 
  Sphere
    { sCenter :: V3 Float
    , sRadius :: Float
    }
  deriving (Eq, Show)

data SceneObject = Either Mesh Sphere
  deriving (Eq, Show)


scalarToFloat :: Scalar -> Float
scalarToFloat (Sfloat x) = x
-- scalarToFloat _          = fail "expected Float value in Scalar"

scalarToInt :: Scalar -> Int
scalarToInt (Suint x) = fromIntegral x
-- scalarToInt _         = fail "expected Int value in Scalar"

plyVertToVertex :: Vec.Vector Scalar -> VertexAttrib
plyVertToVertex vert = VertexAttrib vPos vNorm
  where vertF = Vec.map scalarToFloat vert
        vPos  = V3 (vertF ! 0) (vertF ! 1) (vertF ! 2)
        vNorm = V3 (vertF ! 3) (vertF ! 4) (vertF ! 5)

plyFaceToIndex :: Vec.Vector Scalar -> V3 Int
plyFaceToIndex face = V3 (faceI ! 0) (faceI ! 1) (faceI ! 2)
  where faceI = Vec.map scalarToInt face


loadMesh :: FilePath -> IO Mesh
loadMesh file = do Right vs <- loadElements "vertex" file
                   let verts = Vec.map plyVertToVertex vs
                   Right is <- loadElements "face" file
                   let inds = toList $ Vec.map plyFaceToIndex is
                   return $ Mesh verts inds


applyTransformPoint :: M44 Float -> V3 Float -> V3 Float
-- applyTransformPoint matrix = normalizePoint . (*! matrix) . point
applyTransformPoint matrix = normalizePoint . (matrix !*) . point

applyTransformMesh :: M44 Float -> Mesh -> Mesh
applyTransformMesh matrix (Mesh vs is) = Mesh (Vec.map transformFunc vs) is
  where transformFunc (VertexAttrib pos norm) = VertexAttrib (applyTransformPoint matrix pos) norm

mkTriangles :: Mesh -> MaterialDesc -> [Triangle]
mkTriangles (Mesh vs is) mat = aux vs is
  where aux :: Vec.Vector VertexAttrib -> [V3 Int] -> [Triangle]
        aux verts []              = []
        aux verts ((V3 x y z):is) = Triangle (verts ! x) (verts ! y) (verts ! z) mat : aux verts is

objectDescToTriangles :: M44 Float -> ObjectDesc -> IO [Triangle]
objectDescToTriangles view od = do mesh <- loadMesh $ oPath od
                                   let world = worldMatrix $ oTransform od
                                      --  meshCam = applyTransformMesh (world !*! view) mesh
                                       meshCam = applyTransformMesh (view !*! world) mesh
                                   putStrLn $ show world
                                   return $ mkTriangles meshCam $ oMaterial od
