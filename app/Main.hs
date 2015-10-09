module Main where

import Control.Applicative ((<$>), (<*>))
import GHCJS.Three
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Node

import Data.Maybe (fromJust)

main = runWebGUI $ \webview -> do
  scene <- mkScene
  camera <- mkPerspectiveCamera 45 1 0.1 1000
  renderer <- mkWebGLRenderer
  setSize 500 500 renderer
  glElem <- domElement renderer

  document <- fromJust <$> webViewGetDomDocument webview
  docBody <- fromJust <$> getBody document
  appendChild docBody glElem

  geo <- mkBoxGeometry 1 1 1
  mat <- mkMeshNormalMaterial
  c <- mkColor 0.2 0.3 0.5
  setColor c mat
  cube <- mkMesh geo mat
  add cube scene

  let cp = position camera
  setZ 5 cp
  setPosition cp camera

  let rf = do
         let cubeR = rotation cube
         setX (getX cubeR + 0.01) cubeR
         setY (getY cubeR + 0.01) cubeR
         render scene camera renderer
  runThree rf
