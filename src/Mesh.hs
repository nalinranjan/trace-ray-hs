{-# LANGUAGE OverloadedStrings #-}

module Mesh where

import           MatrixMath
import           Scene
import           Objects

import           Data.Vector   as Vec
import           Linear
import           Linear.Matrix
import           PLY
import           PLY.Types


data Mesh =
  Mesh
    { mVertices :: Vec.Vector VertexAttrib
    , mIndices  :: [V3 Int]
    }
  deriving (Eq, Show)


scalarToFloat :: Scalar -> Float
scalarToFloat (Sfloat x) = x
scalarToFloat _          = error "expected Float value in Scalar"

scalarToInt :: Scalar -> Int
scalarToInt (Suint x) = fromIntegral x
scalarToInt (Sint x)  = fromIntegral x
scalarToInt _         = error "expected Int value in Scalar"

plyVertToVertex :: Vec.Vector Scalar -> VertexAttrib
plyVertToVertex vert = VertexAttrib vPos vNorm
  where
    vertF = Vec.map scalarToFloat vert
    vPos = V3 (vertF ! 0) (vertF ! 1) (vertF ! 2)
    vNorm = V3 (vertF ! 3) (vertF ! 4) (vertF ! 5)

plyFaceToIndex :: Vec.Vector Scalar -> V3 Int
plyFaceToIndex face = V3 (faceI ! 0) (faceI ! 1) (faceI ! 2)
  where
    faceI = Vec.map scalarToInt face

loadMesh :: FilePath -> IO Mesh
loadMesh file = do
  Right vs <- loadElements "vertex" file
  let verts = Vec.map plyVertToVertex vs
  Right is <- loadElements "face" file
  let inds = toList $ Vec.map plyFaceToIndex is
  return $ Mesh verts inds

applyTransformMesh :: M44 Float -> Mesh -> Mesh
applyTransformMesh matrix (Mesh vs is) = Mesh (Vec.map transformFunc vs) is
  where
    transformFunc (VertexAttrib pos norm) =
      VertexAttrib (applyTransformPoint matrix pos) norm

mkTriangles :: Mesh -> MaterialDesc -> [Triangle]
mkTriangles (Mesh vs is) mat = aux vs is
  where
    aux :: Vec.Vector VertexAttrib -> [V3 Int] -> [Triangle]
    aux verts [] = []
    aux verts ((V3 x y z):is) =
      Triangle (verts ! x) (verts ! y) (verts ! z) mat : aux verts is

objectDescToTriangles :: M44 Float -> ObjectDesc -> IO [Triangle]
objectDescToTriangles view od = do
  mesh <- loadMesh $ oPath od
  let world = worldMatrix $ oTransform od
      meshCam = applyTransformMesh (view !*! world) mesh
  return $ mkTriangles meshCam $ oMaterial od
