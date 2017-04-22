module Example2 where

import Prelude
import Graphics.WebGLAll (Mat4, Uniform, Vec3, Attribute, Capacity(DEPTH_TEST), Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT), Mode(TRIANGLE_STRIP, TRIANGLES), Shaders(Shaders), drawArr, bindBufAndSetVertexAttr, setUniformFloats, clear, viewport, getCanvasHeight, getCanvasWidth, makeBufferFloat, enable, clearColor, withShaders, runWebGL)
import Data.Matrix4 (translate, identity, makePerspective) as M
import Data.Matrix (toArray) as M
import Data.Vector3 as V3
import Control.Monad.Eff.Alert (Alert, alert)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Int (toNumber)


shaders :: Shaders {aVertexPosition :: Attribute Vec3, aVertexColor :: Attribute Vec3,
                      uPMatrix :: Uniform Mat4, uMVMatrix:: Uniform Mat4}
shaders = Shaders

  """precision mediump float;

  varying vec4 vColor;

  void main(void) {
    gl_FragColor = vColor;
      }
  """

  """
      attribute vec3 aVertexPosition;
      attribute vec4 aVertexColor;

      uniform mat4 uMVMatrix;
      uniform mat4 uPMatrix;

      varying vec4 vColor;

      void main(void) {
          gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
          vColor = aVertexColor;
      }
  """

main :: Eff (console :: CONSOLE, alert :: Alert) Unit
main =
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        log "WebGL started"
        withShaders shaders
                    (\s -> alert s)
          \ bindings -> do
            clearColor 0.0 0.0 0.0 1.0
            enable DEPTH_TEST

            buf1 <- makeBufferFloat [0.0,  1.0,  0.0,
                                (-1.0), (-1.0),  0.0,
                                1.0, (-1.0),  0.0]
            buf1Colors <- makeBufferFloat  [
                                1.0, 0.0, 0.0, 1.0,
                                0.0, 1.0, 0.0, 1.0,
                                0.0, 0.0, 1.0, 1.0
                                ]
            buf2 <- makeBufferFloat [1.0,  1.0,  0.0,
                               (-1.0), 1.0,  0.0,
                                1.0, (-1.0),  0.0,
                               (-1.0), (-1.0),  0.0]
            buf2Colors <- makeBufferFloat
                               [0.5, 0.5, 1.0, 1.0,
                               0.5, 0.5, 1.0, 1.0,
                               0.5, 0.5, 1.0, 1.0,
                               0.5, 0.5, 1.0, 1.0]

            canvasWidth <- getCanvasWidth context
            canvasHeight <- getCanvasHeight context
            viewport 0 0 canvasWidth canvasHeight
            clear [COLOR_BUFFER_BIT , DEPTH_BUFFER_BIT]

            let pMatrix = M.makePerspective 45.0 (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
            setUniformFloats bindings.uPMatrix (M.toArray pMatrix)
            let mvMatrix = M.translate  (V3.vec3 (-1.5) 0.0 (-7.0)) M.identity
            setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix)

            bindBufAndSetVertexAttr buf1Colors bindings.aVertexColor
            drawArr TRIANGLES buf1 bindings.aVertexPosition

            let mvMatrix' = M.translate (V3.vec3 3.0 0.0 0.0) mvMatrix
            setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix')

            bindBufAndSetVertexAttr buf2Colors bindings.aVertexColor
            drawArr TRIANGLE_STRIP buf2 bindings.aVertexPosition

            log "WebGL completed"
