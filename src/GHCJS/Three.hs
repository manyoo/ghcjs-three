module GHCJS.Three
    (
     module GHCJS.Three.Monad,
     module GHCJS.Three.Matrix,
     module GHCJS.Three.Vector,
     module GHCJS.Three.Euler,
     module GHCJS.Three.Face3,
     module GHCJS.Three.Camera,
     module GHCJS.Three.Projection,
     module GHCJS.Three.Object3D,
     module GHCJS.Three.WebGLRenderer,
     module GHCJS.Three.Scene,
     module GHCJS.Three.Geometry,
     module GHCJS.Three.Material,
     module GHCJS.Three.Mesh,
     module GHCJS.Three.Line,
     module GHCJS.Three.Color,
     module GHCJS.Three.Light,
     module GHCJS.Three.Raycaster,
     module GHCJS.Three.GLNode,
     module GHCJS.Three.Disposable,
     module GHCJS.Three.Visible,
     module GHCJS.Three.HasXYZ,
     module GHCJS.Three.Texture,
     module GHCJS.Three.Path,
     module GHCJS.Three.Shape,
     module GHCJS.Three.ShapeGeometry,
     module GHCJS.Three.CylinderGeometry,
     module GHCJS.Three.CameraHelper,
     runThree
    )
    where

import GHCJS.Three.Monad
import GHCJS.Three.Matrix
import GHCJS.Three.Vector
import GHCJS.Three.Euler
import GHCJS.Three.Face3
import GHCJS.Three.Camera
import GHCJS.Three.Projection
import GHCJS.Three.Object3D
import GHCJS.Three.WebGLRenderer
import GHCJS.Three.Scene
import GHCJS.Three.Geometry
import GHCJS.Three.Material
import GHCJS.Three.Mesh
import GHCJS.Three.Line
import GHCJS.Three.Color
import GHCJS.Three.Light
import GHCJS.Three.Raycaster
import GHCJS.Three.GLNode
import GHCJS.Three.Disposable
import GHCJS.Three.Visible
import GHCJS.Three.HasXYZ
import GHCJS.Three.Texture
import GHCJS.Three.Path
import GHCJS.Three.Shape
import GHCJS.Three.ShapeGeometry
import GHCJS.Three.CylinderGeometry
import GHCJS.Three.CameraHelper

import JavaScript.Web.AnimationFrame
import GHCJS.Foreign.Callback (OnBlocked(..))

import Control.Monad.IO.Class

-- | runThree will run a Three () action in each AnimationFrame and register for the next one
-- so it will rerender the scene on every animation frame.
runThree :: MonadIO m => Three () -> m ()
runThree t = liftIO $ inAnimationFrame ThrowWouldBlock (\ft -> t >> runThree t) >> return ()
