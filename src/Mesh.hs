{-# LANGUAGE OverloadedStrings #-}

module Mesh where

import           MatrixMath
import           Scene
import           Objects

import           Data.Vector   as Vec
import           Linear
import           Linear.Matrix
import           Linear.Metric
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
    vNorm = normalize $ V3 (vertF ! 3) (vertF ! 4) (vertF ! 5)

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
      Triangle v0 v1 v2 mat n : aux verts is
      where v0 = verts ! x
            v1 = verts ! y
            v2 = verts ! z
            n  = normalize $ ((vNormal v0) + (vNormal v1) + (vNormal v2)) / 3

objectDescToTriangles :: M44 Float -> ObjectDesc -> IO [Triangle]
-- objectDescToTriangles view od = do
  -- do mesh <- loadMesh $ oPath od
  --    let world = worldMatrix $ oTransform od
  --        meshCam = applyTransformMesh (view !*! world) mesh
  --    return $ mkTriangles meshCam $ oMaterial od
objectDescToTriangles view (ODMesh path mat tform) = 
  do mesh <- loadMesh $ path
     let world = worldMatrix $ tform
         meshCam = applyTransformMesh (view !*! world) mesh
     return $ mkTriangles meshCam $ mat
objectDescToTriangles _ _ = error "Invalid Object Description"


objectDescToSphere :: M44 Float -> ObjectDesc -> Sphere
-- objectDescToSphere view od = Sphere c (oRadius od) (oMaterial od)
--   where c = applyTransformPoint view $ oCenter od
objectDescToSphere view (ODSphere mat c r) = Sphere cen r mat
  where cen = applyTransformPoint view $ c
objectDescToSphere _    _                  = error "Invalid Object Description"

objectDescsToObjects :: M44 Float -> [ObjectDesc] -> IO [Object]
objectDescsToObjects _    []     = return []
objectDescsToObjects view (o:os) = 
  case o of
    od@(ODMesh _ _ _)   -> do tris <- objectDescToTriangles view od
                              let triObjs = Prelude.map Object tris
                              objs <- objectDescsToObjects view os
                              return $ triObjs Prelude.++ objs
    od@(ODSphere _ _ _) -> do let sph = Object $ objectDescToSphere view od
                              objs <- objectDescsToObjects view os
                              return $ [sph] Prelude.++ objs
