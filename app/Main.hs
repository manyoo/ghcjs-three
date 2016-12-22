module Main where

import Control.Applicative ((<$>), (<*>))
import GHCJS.Three
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Node

import Data.Maybe (fromJust)

main = runWebGUI $ \webview -> do
  -- setup scene, camera and renderer
  scene <- mkScene
  camera <- mkPerspectiveCamera 45 1 0.1 1000
  renderer <- mkWebGLRenderer []
  setSize 500 500 renderer
  glElem <- domElement renderer

  -- setup the DOM document and append the gl canvas to 'body'
  document <- fromJust <$> webViewGetDomDocument webview
  docBody <- fromJust <$> getBody document
  appendChild docBody glElem

  -- setup contents in the scene
  geo <- mkBoxGeometry 1 1 1
  mat <- mkMeshNormalMaterial
  c <- mkColor 0.2 0.3 0.5
  setColor c mat
  cube <- mkMesh geo mat
  add (toGLNode cube) (toGLNode scene)

  cp <- position camera
  setPosition cp {v3z = 5} camera

  -- this is the action for each animation frame, change the rotation of the
  -- cube and render it.
  let rf = do
         cubeR <- rotation cube
         setRotation cubeR {eX = eX cubeR + 0.01, eY = eY cubeR + 0.01} cube
         render scene camera renderer
  runThree rf
