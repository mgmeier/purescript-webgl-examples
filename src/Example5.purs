
module Example5 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import System.Clock (CLOCK, milliseconds)
import Data.Maybe (Maybe(Just, Nothing))
import Math (pi)
import Data.Int (toNumber)

import Graphics.WebGLAll (EffWebGL, WebGLTex, Buffer, Sampler2D, Uniform, Mat4, Vec2, Attribute, Vec3, WebGLProg, WebGLContext,
                        BufferTarget(ELEMENT_ARRAY_BUFFER), Capacity(DEPTH_TEST), Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT),
                        Mode(TRIANGLES), Shaders(Shaders), TexFilterSpec(MIPMAP), drawElements, bindBuf, withTexture2D,
                        bindBufAndSetVertexAttr, setUniformFloats, clear, viewport, getCanvasHeight, getCanvasWidth,
                        requestAnimationFrame, texture2DFor, enable, clearColor, makeBuffer, makeBufferFloat, withShaders, runWebGL)
import Data.Matrix4 (identity, translate, rotate, makePerspective) as M
import Data.Matrix (toArray) as M
import Data.Vector3 as V3
import Control.Monad.Eff.Alert (Alert, alert)
import Data.ArrayBuffer.Types (Uint16, Float32) as T
import Data.TypedArray (asUint16Array) as T


shaders :: Shaders {aVertexPosition :: Attribute Vec3, aTextureCoord :: Attribute Vec2,
                      uPMatrix :: Uniform Mat4, uMVMatrix:: Uniform Mat4, uSampler :: Uniform Sampler2D}
shaders = Shaders

  """ precision mediump float;

      varying vec2 vTextureCoord;

      uniform sampler2D uSampler;

      void main(void) {
          gl_FragColor = texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t));
      }
  """

  """
      attribute vec3 aVertexPosition;
      attribute vec2 aTextureCoord;

      uniform mat4 uMVMatrix;
      uniform mat4 uPMatrix;

      varying vec2 vTextureCoord;


      void main(void) {
          gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
          vTextureCoord = aTextureCoord;
      }
  """

type State =
    {
        context :: WebGLContext,
        shaderProgram :: WebGLProg,

        aVertexPosition :: Attribute Vec3,
        aTextureCoord :: Attribute Vec2,
        uPMatrix :: Uniform Mat4,
        uMVMatrix :: Uniform Mat4,
        uSampler :: Uniform Sampler2D,

        cubeVertices :: Buffer T.Float32,
        textureCoords :: Buffer T.Float32,
        cubeVertexIndices :: Buffer T.Uint16,
        texture :: WebGLTex,

        lastTime :: Maybe Number,
        rot :: Number
    }

main :: Eff (console :: CONSOLE, alert :: Alert, clock :: CLOCK) Unit
main = do
--  let stupid = Mat3 "stupid"
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        log "WebGL started"
        withShaders shaders
                    (\s -> alert s)
                      \ binding -> do
          cubeVertices <- makeBufferFloat [
                            -- Front face
                            -1.0, -1.0,  1.0,
                             1.0, -1.0,  1.0,
                             1.0,  1.0,  1.0,
                            -1.0,  1.0,  1.0,

                            -- Back face
                            -1.0, -1.0, -1.0,
                            -1.0,  1.0, -1.0,
                             1.0,  1.0, -1.0,
                             1.0, -1.0, -1.0,

                            -- Top face
                            -1.0,  1.0, -1.0,
                            -1.0,  1.0,  1.0,
                             1.0,  1.0,  1.0,
                             1.0,  1.0, -1.0,

                            -- Bottom face
                            -1.0, -1.0, -1.0,
                             1.0, -1.0, -1.0,
                             1.0, -1.0,  1.0,
                            -1.0, -1.0,  1.0,

                            -- Right face
                             1.0, -1.0, -1.0,
                             1.0,  1.0, -1.0,
                             1.0,  1.0,  1.0,
                             1.0, -1.0,  1.0,

                            -- Left face
                            -1.0, -1.0, -1.0,
                            -1.0, -1.0,  1.0,
                            -1.0,  1.0,  1.0,
                            -1.0,  1.0, -1.0]

          textureCoords <- makeBufferFloat [
                            -- Front face
                            0.0, 0.0,
                            1.0, 0.0,
                            1.0, 1.0,
                            0.0, 1.0,

                            -- Back face
                            1.0, 0.0,
                            1.0, 1.0,
                            0.0, 1.0,
                            0.0, 0.0,

                            -- Top face
                            0.0, 1.0,
                            0.0, 0.0,
                            1.0, 0.0,
                            1.0, 1.0,

                            -- Bottom face
                            1.0, 1.0,
                            0.0, 1.0,
                            0.0, 0.0,
                            1.0, 0.0,

                            -- Right face
                            1.0, 0.0,
                            1.0, 1.0,
                            0.0, 1.0,
                            0.0, 0.0,

                            -- Left face
                            0.0, 0.0,
                            1.0, 0.0,
                            1.0, 1.0,
                            0.0, 1.0
                          ]
          cubeVertexIndices <- makeBuffer ELEMENT_ARRAY_BUFFER T.asUint16Array
                          [
                            0, 1, 2,      0, 2, 3,    -- Front face
                            4, 5, 6,      4, 6, 7,    -- Back face
                            8, 9, 10,     8, 10, 11,  -- Top face
                            12, 13, 14,   12, 14, 15, -- Bottom face
                            16, 17, 18,   16, 18, 19, -- Right face
                            20, 21, 22,   20, 22, 23  -- Left face
                          ]
          clearColor 0.0 0.0 0.0 1.0
          enable DEPTH_TEST
          texture2DFor "test.png" MIPMAP \texture ->
            tick {
                  context : context,
                  shaderProgram : binding.webGLProgram,

                  aVertexPosition : binding.aVertexPosition,
                  aTextureCoord : binding.aTextureCoord,
                  uPMatrix : binding.uPMatrix,
                  uMVMatrix : binding.uMVMatrix,
                  uSampler : binding.uSampler,

                  cubeVertices : cubeVertices,
                  textureCoords : textureCoords,
                  cubeVertexIndices : cubeVertexIndices,
                  texture : texture,
                  lastTime : Nothing,
                  rot : 0.0
                }


tick :: forall eff. State  ->  EffWebGL (console :: CONSOLE, clock :: CLOCK |eff) Unit
tick state = do
--  log ("tick: " ++ show state.lastTime)
  drawScene state
  state' <- animate state
  requestAnimationFrame (tick state')

animate ::  forall eff. State -> EffWebGL (clock :: CLOCK |eff) State
animate state = do
  timeNow <- milliseconds
  case state.lastTime of
    Nothing -> pure state {lastTime = Just timeNow}
    Just lastt ->
      let elapsed = timeNow - lastt
      in pure state {lastTime = Just timeNow,
                       rot = state.rot + (90.0 * elapsed) / 1000.0
                       }

drawScene :: forall eff. State -> EffWebGL (clock :: CLOCK | eff) Unit
drawScene s = do
      canvasWidth <- getCanvasWidth s.context
      canvasHeight <- getCanvasHeight s.context
      viewport 0 0 canvasWidth canvasHeight
      clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

      let pMatrix = M.makePerspective 45.0 (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
      setUniformFloats s.uPMatrix (M.toArray pMatrix)

      let mvMatrix = M.rotate (degToRad s.rot) (V3.vec3' [1.0, 0.0, 0.0])
                        $ M.rotate (degToRad s.rot) (V3.vec3' [0.0, 1.0, 0.0])
                          $ M.rotate (degToRad s.rot) (V3.vec3' [0.0, 0.0, 1.0])
                            $ M.translate  (V3.vec3 0.0 0.0 (-8.0))
                              $ M.identity

      setUniformFloats s.uMVMatrix (M.toArray mvMatrix)

      bindBufAndSetVertexAttr s.cubeVertices s.aVertexPosition
      bindBufAndSetVertexAttr s.textureCoords s.aTextureCoord

      withTexture2D s.texture 0 s.uSampler 0 (do
          bindBuf s.cubeVertexIndices
          drawElements TRIANGLES s.cubeVertexIndices.bufferSize
      )

-- | Convert from radians to degrees.
radToDeg :: Number -> Number
radToDeg x = x/pi*180.0

-- | Convert from degrees to radians.
degToRad :: Number -> Number
degToRad x = x/180.0*pi
