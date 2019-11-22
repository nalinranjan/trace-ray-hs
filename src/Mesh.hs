{-# LANGUAGE OverloadedStrings   #-}

module Mesh where

import           Scene

import           Linear
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
    , mIndices  :: Vec.Vector (V3 Int) 
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
                   let inds = Vec.map plyFaceToIndex is
                   return $ Mesh verts inds
