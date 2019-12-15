{-# OPTIONS -Wall #-}

module MeshTests where

import           Mesh
import           Scene
import           Objects
import           Data.Colour.SRGB.Linear

import           System.IO
import           Test.HUnit
import           Linear

meshTests :: IO()   
meshTests = do hPutStrLn stdout "\n\nMesh Tests:\n"
               hPutStrLn stderr "##########"
               mesh <- sequence $ map loadMeshTests loadMeshCases
               tri  <- sequence $ map mkTriTests mkTriTestCases
               objs <- sequence $ map objectDescsToObjectsTests objectDescsToObjectsTestCases
               hPutStrLn stdout "loadMesh Tests:"
               _ <- runTestTT (TestList mesh)
               hPutStrLn stdout "mkTriangles Tests:"
               _ <- runTestTT (TestList tri)
               hPutStrLn stdout "objectDescsToObjects Tests:"
               _ <- runTestTT (TestList objs)
               return ()

loadMeshCases :: [([Char], [Int])]
loadMeshTests :: (String, [Int]) -> IO Test 
loadMeshTests (file, ans) = do (Mesh verts inds) <- loadMesh file
                               return $ [length verts, length inds] ~?= ans

mkTriTests :: (String, [Triangle]) -> IO Test
mkTriTests (file, ans) = do m <- loadMesh file
                            return $ mkTriangles m dummyMaterial ~?= ans
                            where dummyMaterial = (MaterialDesc 
                                                    {mDiffuseColor = RGB {channelRed = 0.52, 
                                                                          channelGreen = 0.27,
                                                                          channelBlue = 0.15},
                                                     mKr = 0.0,
                                                     mKt = 0.0,
                                                     mAlpha = 20.0,
                                                     mIoR = 1})

loadMeshCases = [("meshes/plane_bottom.ply", [4, 2]),
                 ("meshes/cube.ply", [8, 12]),
                 ("meshes/dragon.ply", [354948, 709988]),
                 ("meshes/bunny.ply", [34834, 69451]),
                 ("meshes/earth.ply", [499, 960]),
                 ("meshes/monkey.ply", [2944, 5802]),
                 ("meshes/table.ply", [5890, 11776])]

mkTriTestCases :: [(String, [Triangle])]
mkTriTestCases = 
  [("meshes/plane_bottom.ply", [Triangle {tV0 = VertexAttrib {vPosition = V3 6.659805 (-2.204e-3)
  (-5.507288), vNormal = V3 0.0 1.0 0.0}, tV1 = VertexAttrib {vPosition = V3 (-6.752673) (-2.204e-3) (-5.507288),
  vNormal = V3 0.0 1.0 0.0}, tV2 = VertexAttrib {vPosition = V3 (-6.752673) (-2.204e-3) 5.684772, 
  vNormal = V3 0.0 1.0 0.0}, tMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, 
  channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0}, tNormal = V3 0.0 1.0 0.0},Triangle 
  {tV0 = VertexAttrib {vPosition = V3 6.659805 (-2.204e-3) 5.684772, vNormal = V3 0.0 1.0 0.0}, 
  tV1 = VertexAttrib {vPosition = V3 6.659805 (-2.204e-3) (-5.507288), vNormal = V3 0.0 1.0 0.0}, 
  tV2 = VertexAttrib {vPosition = V3 (-6.752673) (-2.204e-3) 5.684772, vNormal = V3 0.0 1.0 0.0}, 
  tMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, 
  mKr = 0.0, mKt = 0.0, mAlpha = 20.0}, tNormal = V3 0.0 1.0 0.0}]),
  ("meshes/plane_back.ply", [Triangle {tV0 = VertexAttrib {vPosition = V3 (-8.410525) 9.59385 (-5.38884), 
  vNormal = V3 (-1.0e-6) 0.0 1.0}, tV1 = VertexAttrib {vPosition = V3 (-8.410525) (-0.267121) (-5.38884), 
  vNormal = V3 (-1.0e-6) 0.0 1.0}, tV2 = VertexAttrib {vPosition = V3 8.634561 (-0.267122) (-5.388831), 
  vNormal = V3 (-1.0e-6) 0.0 1.0}, tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, 
  channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 (-5.31526e-7) 0.0 1.0},
  Triangle {tV0 = VertexAttrib {vPosition = V3 8.634561 9.593849 (-5.388831), vNormal = V3 (-1.0e-6) 0.0 1.0}, 
  tV1 = VertexAttrib {vPosition = V3 (-8.410525) 9.59385 (-5.38884), vNormal = V3 (-1.0e-6) 0.0 1.0}, 
  tV2 = VertexAttrib {vPosition = V3 8.634561 (-0.267122) (-5.388831), vNormal = V3 (-1.0e-6) 0.0 1.0}, 
  tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, 
  mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 (-5.31526e-7) 0.0 1.0}]),
  ("meshes/plane_front.ply", [Triangle {tV0 = VertexAttrib {vPosition = V3 8.634561 (-0.267121) 5.836553, 
  vNormal = V3 0.0 0.0 (-1.0)}, tV1 = VertexAttrib {vPosition = V3 (-8.410525) (-0.267122) 5.836547,
  vNormal = V3 0.0 0.0 (-1.0)}, tV2 = VertexAttrib {vPosition = V3 (-8.410525) 9.593849 5.836547, 
  vNormal = V3 0.0 0.0 (-1.0)}, tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, 
  channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 3.6367567e-7 0.0 (-1.0)},
  Triangle {tV0 = VertexAttrib {vPosition = V3 8.634561 9.59385 5.836553, vNormal = V3 0.0 0.0 (-1.0)}, 
  tV1 = VertexAttrib {vPosition = V3 8.634561 (-0.267121) 5.836553, vNormal = V3 0.0 0.0 (-1.0)}, 
  tV2 = VertexAttrib {vPosition = V3 (-8.410525) 9.593849 5.836547, vNormal = V3 0.0 0.0 (-1.0)}, 
  tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, 
  mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 3.6367567e-7 0.0 (-1.0)}]),
  ("meshes/plane_left.ply", [Triangle {tV0 = VertexAttrib {vPosition = V3 (-6.460826) 9.593849 5.693579, 
  vNormal = V3 1.0 0.0 0.0}, tV1 = VertexAttrib {vPosition = V3 (-6.460826) (-0.267122) 5.693579, 
  vNormal = V3 1.0 0.0 0.0}, tV2 = VertexAttrib {vPosition = V3 (-6.460826) (-0.267122) (-5.542285), 
  vNormal = V3 1.0 0.0 0.0}, tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, 
  channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 1.0 0.0 0.0},
  Triangle {tV0 = VertexAttrib {vPosition = V3 (-6.460825) 9.593849 (-5.542285), vNormal = V3 1.0 0.0 0.0}, 
  tV1 = VertexAttrib {vPosition = V3 (-6.460826) 9.593849 5.693579, vNormal = V3 1.0 0.0 0.0}, 
  tV2 = VertexAttrib {vPosition = V3 (-6.460826) (-0.267122) (-5.542285), vNormal = V3 1.0 0.0 0.0}, 
  tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, 
  mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 1.0 (-9.671201e-8) 8.48777e-8}]),
  ("meshes/plane_right.ply", [Triangle {tV0 = VertexAttrib {vPosition = V3 5.808484 9.593849 (-5.555173), 
  vNormal = V3 (-1.0) 0.0 0.0}, tV1 = VertexAttrib {vPosition = V3 5.808483 (-0.267122) (-5.555173), 
  vNormal = V3 (-1.0) 0.0 0.0}, tV2 = VertexAttrib {vPosition = V3 5.808481 (-0.267122) 5.680691, 
  vNormal = V3 (-1.0) 0.0 0.0}, tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, 
  channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, 
  tNormal = V3 (-1.0) 9.671201e-8 (-1.6975541e-7)},Triangle {tV0 = VertexAttrib {
  vPosition = V3 5.808482 9.593849 5.680691, vNormal = V3 (-1.0) 0.0 0.0}, tV1 = VertexAttrib {
  vPosition = V3 5.808484 9.593849 (-5.555173), vNormal = V3 (-1.0) 0.0 0.0}, tV2 = VertexAttrib {
  vPosition = V3 5.808481 (-0.267122) 5.680691, vNormal = V3 (-1.0) 0.0 0.0}, tMat = MaterialDesc {
  mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 (-1.0) 9.671201e-8 (-1.6975541e-7)}]),
  ("meshes/plane_top.ply", [Triangle {tV0 = VertexAttrib {vPosition = V3 6.659805 8.683173 5.684772, 
  vNormal = V3 0.0 (-1.0) 0.0}, tV1 = VertexAttrib {vPosition = V3 (-6.752673) 8.683173 5.684772, 
  vNormal = V3 0.0 (-1.0) 0.0}, tV2 = VertexAttrib {vPosition = V3 (-6.752673) 8.683175 (-5.507288), 
  vNormal = V3 0.0 (-1.0) 0.0}, tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, 
  channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 (-0.0) (-1.0) (-1.7041978e-7)},
  Triangle {tV0 = VertexAttrib {vPosition = V3 6.659805 8.683175 (-5.507288), vNormal = V3 0.0 (-1.0) 0.0}, 
  tV1 = VertexAttrib {vPosition = V3 6.659805 8.683173 5.684772, vNormal = V3 0.0 (-1.0) 0.0}, 
  tV2 = VertexAttrib {vPosition = V3 (-6.752673) 8.683175 (-5.507288), vNormal = V3 0.0 (-1.0) 0.0}, 
  tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, 
  mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 (-0.0) (-1.0) (-1.7041978e-7)}]),
  ("meshes/wall.ply", [Triangle {tV0 = VertexAttrib {vPosition = V3 (-7.830082) 16.604795 (-10.20876), vNormal = 
  V3 1.0 0.0 0.0}, tV1 = VertexAttrib {vPosition = V3 (-7.830084) 16.604795 10.68427, vNormal = V3 1.0 0.0 0.0}, 
  tV2 = VertexAttrib {vPosition = V3 (-7.830082) (-4.288235) 10.68427, vNormal = V3 1.0 0.0 0.0}, tMat = 
  MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, 
  mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 1.0 9.129114e-8 9.129114e-8},Triangle {tV0 = VertexAttrib {
  vPosition = V3 (-7.830081) (-4.288235) (-10.20876), vNormal = V3 1.0 0.0 0.0}, tV1 = VertexAttrib {vPosition = 
  V3 (-7.830082) 16.604795 (-10.20876), vNormal = V3 1.0 0.0 0.0}, tV2 = VertexAttrib {
  vPosition = V3 (-7.830082) (-4.288235) 10.68427, vNormal = V3 1.0 0.0 0.0}, tMat = MaterialDesc {
  mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 1.0 4.564557e-8 4.564557e-8}]),
  ("meshes/cube.ply", [Triangle {tV0 = VertexAttrib {vPosition = V3 1.0 1.0 (-1.0), 
  vNormal = V3 0.57735026 0.57735026 (-0.57735026)}, tV1 = VertexAttrib {vPosition = V3 1.0 (-1.0) (-1.0), 
  vNormal = V3 0.57735026 (-0.57735026) (-0.57735026)}, tV2 = VertexAttrib {vPosition = V3 (-1.0) (-1.0) (-1.0), 
  vNormal = V3 (-0.57735026) (-0.57735026) (-0.57735026)}, tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52,
  channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, 
  tNormal = V3 0.0 (-0.0) (-1.0)},Triangle {tV0 = VertexAttrib {vPosition = V3 (-1.0) 1.0 1.0, 
  vNormal = V3 (-0.57735026) 0.57735026 0.57735026}, tV1 = VertexAttrib {vPosition = V3 (-1.0) (-1.0) 1.0, 
  vNormal = V3 (-0.57735026) (-0.57735026) 0.57735026}, tV2 = VertexAttrib {vPosition = V3 0.999999 (-1.000001) 1.0, 
  vNormal = V3 0.57735026 (-0.57735026) 0.57735026}, tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, 
  channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 0.0 0.0 1.0},
  Triangle {tV0 = VertexAttrib {vPosition = V3 1.0 0.999999 1.0, vNormal = V3 0.57735026 0.57735026 0.57735026}, 
  tV1 = VertexAttrib {vPosition = V3 0.999999 (-1.000001) 1.0, vNormal = V3 0.57735026 (-0.57735026) 0.57735026}, 
  tV2 = VertexAttrib {vPosition = V3 1.0 (-1.0) (-1.0), vNormal = V3 0.57735026 (-0.57735026) (-0.57735026)}, 
  tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, 
  mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 1.0 (-5.066395e-7) 5.0663925e-7},
  Triangle {tV0 = VertexAttrib {vPosition = V3 0.999999 (-1.000001) 1.0, vNormal = V3 0.57735026 (-0.57735026) 0.57735026}, 
  tV1 = VertexAttrib {vPosition = V3 (-1.0) (-1.0) 1.0, vNormal = V3 (-0.57735026) (-0.57735026) 0.57735026}, 
  tV2 = VertexAttrib {vPosition = V3 (-1.0) (-1.0) (-1.0), vNormal = V3 (-0.57735026) (-0.57735026) (-0.57735026)}, 
  tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, 
  mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 (-4.768374e-7) (-1.0) 0.0},Triangle {tV0 = VertexAttrib {
  vPosition = V3 (-1.0) (-1.0) (-1.0), vNormal = V3 (-0.57735026) (-0.57735026) (-0.57735026)}, tV1 = VertexAttrib {
  vPosition = V3 (-1.0) (-1.0) 1.0, vNormal = V3 (-0.57735026) (-0.57735026) 0.57735026}, tV2 = VertexAttrib {
  vPosition = V3 (-1.0) 1.0 1.0, vNormal = V3 (-0.57735026) 0.57735026 0.57735026}, tMat = MaterialDesc {
  mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 (-1.0) 0.0 0.0},Triangle {tV0 = VertexAttrib {
  vPosition = V3 1.0 0.999999 1.0, vNormal = V3 0.57735026 0.57735026 0.57735026}, tV1 = VertexAttrib {
  vPosition = V3 1.0 1.0 (-1.0), vNormal = V3 0.57735026 0.57735026 (-0.57735026)}, tV2 = VertexAttrib {
  vPosition = V3 (-1.0) 1.0 (-1.0), vNormal = V3 (-0.57735026) 0.57735026 (-0.57735026)}, tMat = MaterialDesc {
  mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 0.0 1.0 5.066395e-7},Triangle {tV0 = VertexAttrib {
  vPosition = V3 (-1.0) 1.0 (-1.0), vNormal = V3 (-0.57735026) 0.57735026 (-0.57735026)}, tV1 = VertexAttrib {
  vPosition = V3 1.0 1.0 (-1.0), vNormal = V3 0.57735026 0.57735026 (-0.57735026)}, tV2 = VertexAttrib {
  vPosition = V3 (-1.0) (-1.0) (-1.0), vNormal = V3 (-0.57735026) (-0.57735026) (-0.57735026)}, tMat = MaterialDesc {
  mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 0.0 0.0 (-1.0)},Triangle {tV0 = VertexAttrib {vPosition = 
  V3 1.0 0.999999 1.0, vNormal = V3 0.57735026 0.57735026 0.57735026}, tV1 = VertexAttrib {vPosition = 
  V3 (-1.0) 1.0 1.0, vNormal = V3 (-0.57735026) 0.57735026 0.57735026}, tV2 = VertexAttrib {vPosition = 
  V3 0.999999 (-1.000001) 1.0, vNormal = V3 0.57735026 (-0.57735026) 0.57735026}, tMat = MaterialDesc {
  mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 0.0 0.0 1.0},Triangle {tV0 = VertexAttrib {vPosition = V3 1.0 1.0 (-1.0), 
  vNormal = V3 0.57735026 0.57735026 (-0.57735026)}, tV1 = VertexAttrib {vPosition = V3 1.0 0.999999 1.0, 
  vNormal = V3 0.57735026 0.57735026 0.57735026}, tV2 = VertexAttrib {vPosition = V3 1.0 (-1.0) (-1.0), 
  vNormal = V3 0.57735026 (-0.57735026) (-0.57735026)}, tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, 
  channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 1.0 0.0 0.0},
  Triangle {tV0 = VertexAttrib {vPosition = V3 1.0 (-1.0) (-1.0), vNormal = V3 0.57735026 (-0.57735026) (-0.57735026)}, 
  tV1 = VertexAttrib {vPosition = V3 0.999999 (-1.000001) 1.0, vNormal = V3 0.57735026 (-0.57735026) 0.57735026}, 
  tV2 = VertexAttrib {vPosition = V3 (-1.0) (-1.0) (-1.0), vNormal = V3 (-0.57735026) (-0.57735026) (-0.57735026)}, 
  tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, 
  mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 (-0.0) (-1.0) (-4.7683716e-7)},Triangle {tV0 = VertexAttrib {
  vPosition = V3 (-1.0) 1.0 (-1.0), vNormal = V3 (-0.57735026) 0.57735026 (-0.57735026)}, tV1 = VertexAttrib {
  vPosition = V3 (-1.0) (-1.0) (-1.0), vNormal = V3 (-0.57735026) (-0.57735026) (-0.57735026)}, tV2 = VertexAttrib {
  vPosition = V3 (-1.0) 1.0 1.0, vNormal = V3 (-0.57735026) 0.57735026 0.57735026}, tMat = MaterialDesc {
  mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0, mIoR = 1.0}, tNormal = V3 (-1.0) 0.0 0.0},Triangle {tV0 = VertexAttrib {vPosition = V3 (-1.0) 1.0 1.0, 
  vNormal = V3 (-0.57735026) 0.57735026 0.57735026}, tV1 = VertexAttrib {vPosition = V3 1.0 0.999999 1.0, 
  vNormal = V3 0.57735026 0.57735026 0.57735026}, tV2 = VertexAttrib {vPosition = V3 (-1.0) 1.0 (-1.0), 
  vNormal = V3 (-0.57735026) 0.57735026 (-0.57735026)}, tMat = MaterialDesc {mDiffuseColor = RGB {channelRed = 0.52, 
  channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0, mIoR = 1.0}, 
  tNormal = V3 5.066395e-7 1.0 0.0}])]

objectDescsToObjectsTests :: (M44 Float, [ObjectDesc], [Object]) -> IO Test
objectDescsToObjectsTests (m44, objDesc, ans) = do result <- objectDescsToObjects m44 objDesc
                                                   return $ length result ~?= length ans

objectDescsToObjectsTestCases :: [(M44 Float, [ObjectDesc], [Object])]
objectDescsToObjectsTestCases = [
  ((V4 (V4 1.0 (-0.0) 0.0 (-0.0)) (V4 0.0 0.9578263 (-0.28734788) (-574.6958)) (V4 (-0.0) 0.28734788 0.9578263 (-172.40874)) 
  (V4 0.0 0.0 0.0 1.0)),
  [ODMesh "meshes/wall.ply" (MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 0.961, channelGreen = 0.851, channelBlue = 0.651}, 
  mKr = 0.0, mKt = 0.0, mAlpha = 20.0}) (TransformDesc {tTranslation = V3 0.0 (-1300.0) (-2000.0), 
  tRotation = Quaternion 1.0 (V3 0.0 0.0 0.0), tScale = V3 350.0 350.0 350.0})], 
  [Object (Triangle {tV0 = VertexAttrib {vPosition = V3 (-2740.5286) 5348.1167 (-4214.0166), vNormal = V3 1.0 0.0 0.0}, 
  tV1 = VertexAttrib {vPosition = V3 (-2740.5293) 3246.8677 2790.1462, vNormal = V3 1.0 0.0 0.0}, 
  tV2 = VertexAttrib {vPosition = V3 (-2740.5286) (-3757.2947) 688.8972, vNormal = V3 1.0 0.0 0.0}, 
  tMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 0.961, channelGreen = 0.851, channelBlue = 0.651}, 
  mKr = 0.0, mKt = 0.0, mAlpha = 20.0}, tNormal = V3 1.0 0.0 0.0}),
  Object (Triangle {tV0 = VertexAttrib {
  vPosition = V3 (-2740.5283) (-1656.0458) (-6315.2656), vNormal = V3 1.0 0.0 0.0}, tV1 = VertexAttrib {
  vPosition = V3 (-2740.5286) 5348.1167 (-4214.0166), vNormal = V3 1.0 0.0 0.0}, tV2 = VertexAttrib {
  vPosition = V3 (-2740.5286) (-3757.2947) 688.8972, vNormal = V3 1.0 0.0 0.0}, tMat = MaterialDesc {mIoR = 1, 
  mDiffuseColor = RGB {channelRed = 0.961, channelGreen = 0.851, channelBlue = 0.651}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0}, tNormal = V3 1.0 0.0 0.0})]),
  ((V4 (V4 (-15831.778) (-1835.68) 1534.4598 650.0) (V4 2044.7201 (-15863.678) 1022.55994 200.0) (V4 1242.3398 1362.64 (-111.45999) (-2000.0)) 
  (V4 0.0 0.0 0.0 1.0)), 
  [ODMesh "meshes/wall.ply" (MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 0.961, channelGreen = 0.851, 
  channelBlue = 0.651}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0}) (TransformDesc {tTranslation = V3 0.0 0.0 (-200.0), 
  tRotation = Quaternion 0.525 (V3 0.0 (-0.851) 0.0), tScale = V3 350.0 350.0 350.0}),
  ODMesh "meshes/plane_bottom.ply" (MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, 
  channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0}) (TransformDesc {tTranslation = V3 0.0 200.0 (-1800.0), 
  tRotation = Quaternion 0.995 (V3 0.0 9.8e-2 0.0), tScale = V3 100.0 100.0 100.0}),ODSphere (MaterialDesc {mIoR = 1, 
  mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0}, mKr = 0.0, mKt = 0.0, mAlpha = 200.0})
  (V3 0.0 (-1000.0) (-3000.0)) 500.0],
  [Object (Triangle {tV0 = VertexAttrib {vPosition = V3 (-8.227509e7) (-8.422375e7) 1.3526978e7, vNormal = V3 1.0 0.0 0.0}, 
  tV1 = VertexAttrib {vPosition = V3 1.6140494e7 (-1.00937176e8) 5774831.0, vNormal = V3 1.0 0.0 0.0}, 
  tV2 = VertexAttrib {vPosition = V3 2.9564024e7 1.5066923e7 (-4189557.0), vNormal = V3 1.0 0.0 0.0},
  tMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 0.961, channelGreen = 0.851, channelBlue = 0.651},
  mKr = 0.0, mKt = 0.0, mAlpha = 20.0}, tNormal = V3 1.0 0.0 0.0}), 
  Object (Triangle {tV0 = VertexAttrib {
  vPosition = V3 (-6.885157e7) 3.1780348e7 3562589.5, vNormal = V3 1.0 0.0 0.0}, tV1 = VertexAttrib {
  vPosition = V3 (-8.227509e7) (-8.422375e7) 1.3526978e7, vNormal = V3 1.0 0.0 0.0}, tV2 = VertexAttrib {
  vPosition = V3 2.9564024e7 1.5066923e7 (-4189557.0), vNormal = V3 1.0 0.0 0.0}, tMat = MaterialDesc {mIoR = 1, 
  mDiffuseColor = RGB {channelRed = 0.961, channelGreen = 0.851, channelBlue = 0.651}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0}, tNormal = V3 1.0 0.0 0.0}),
  Object (Triangle {tV0 = VertexAttrib {vPosition = V3 (-1.2796993e7) (-4578815.0) 1223588.0, 
  vNormal = V3 0.0 1.0 0.0}, tV1 = VertexAttrib {vPosition = V3 8430843.0 (-7001142.5) (-439846.25), 
  vNormal = V3 0.0 1.0 0.0}, tV2 = VertexAttrib {vPosition = V3 6659669.0 (-5432374.0) (-291034.38), 
  vNormal = V3 0.0 1.0 0.0}, tMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, 
  channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, mAlpha = 20.0}, tNormal = V3 0.0 1.0 0.0}),
  Object (Triangle {tV0 = VertexAttrib {
  vPosition = V3 (-1.4568167e7) (-3010046.3) 1372399.8, vNormal = V3 0.0 1.0 0.0}, tV1 = VertexAttrib {
  vPosition = V3 (-1.2796993e7) (-4578815.0) 1223588.0, vNormal = V3 0.0 1.0 0.0}, tV2 = VertexAttrib {
  vPosition = V3 6659669.0 (-5432374.0) (-291034.38), vNormal = V3 0.0 1.0 0.0}, tMat = MaterialDesc {mIoR = 1, 
  mDiffuseColor = RGB {channelRed = 0.52, channelGreen = 0.27, channelBlue = 0.15}, mKr = 0.0, mKt = 0.0, 
  mAlpha = 20.0}, tNormal = V3 0.0 1.0 0.0}), 
  Object (Sphere {sCenter = V3 (-2767049.5) 1.2796198e7 (-1030260.0), 
  sRadius = 500.0, sMat = MaterialDesc {mIoR = 1, mDiffuseColor = RGB {channelRed = 1.0, channelGreen = 0.843, channelBlue = 0.0},
  mKr = 0.0, mKt = 0.0, mAlpha = 200.0}})])]