module Example1 where

import Prelude
import Graphics.WebGLAll (Mat4, Uniform, Vec3, Attribute, Capacity(DEPTH_TEST), Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT), Mode(TRIANGLE_STRIP, TRIANGLES), Shaders(Shaders), drawArr, makeBufferFloat, setUniformFloats, clear, viewport, getCanvasHeight, getCanvasWidth, enable, clearColor, withShaders, runWebGL)
import Data.Matrix4 (translate, identity, makePerspective) as M
import Data.Matrix (toArray) as M
import Data.Vector3 as V
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Alert (Alert, alert)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Int (toNumber)


shaders :: Shaders {aVertexPosition :: Attribute Vec3, uPMatrix :: Uniform Mat4, uMVMatrix:: Uniform Mat4}
shaders = Shaders

  """precision mediump float;

  void main(void) {
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
      }
  """

  """
      attribute vec3 aVertexPosition;

      uniform mat4 uMVMatrix;
      uniform mat4 uPMatrix;

      void main(void) {
          gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
      }
  """

main :: Eff (console :: CONSOLE, alert :: Alert) Unit
main = runWebGL "glcanvas" (\s -> alert s)
  \ context -> do
    log "WebGL ready"
    withShaders shaders (\s -> alert s)
      \ bindings -> do
        log "Shaders and bindings ready"
        clearColor 0.0 0.0 0.0 1.0
        enable DEPTH_TEST

        canvasWidth <- getCanvasWidth context
        canvasHeight <- getCanvasHeight context
        viewport 0 0 canvasWidth canvasHeight
        clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

        let pMatrix = M.makePerspective 45.0 (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
        setUniformFloats bindings.uPMatrix (M.toArray pMatrix)

        let mvMatrix = M.translate (V.vec3 (-1.5) 0.0 (-7.0))
                          M.identity
        setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix)

        buf1 <- makeBufferFloat [0.0,  1.0,  0.0,
                           (-1.0), (-1.0),  0.0,
                            1.0, (-1.0),  0.0]
        drawArr TRIANGLES buf1 bindings.aVertexPosition

        let mvMatrix' = M.translate (V.vec3 3.0 0.0 0.0)
                          mvMatrix
        setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix')

        buf2 <- makeBufferFloat [1.0,  1.0,  0.0,
                           (-1.0), 1.0,  0.0,
                            1.0, (-1.0),  0.0,
                           (-1.0), (-1.0),  0.0]
        drawArr TRIANGLE_STRIP buf2 bindings.aVertexPosition

        log "WebGL completed"
