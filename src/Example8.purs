module Example8 where

import Prelude
import Control.Monad.Eff.Alert (Alert, alert)
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, writeSTRef, readSTRef, newSTRef, runST)
import Control.Monad.Eff.Console (CONSOLE, log)
import System.Clock (CLOCK, milliseconds)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Array (delete, elemIndex, (:), null)
import Math (pi)
import Data.Int (toNumber)
import Partial.Unsafe (unsafePartial)

import Graphics.WebGLAll (EffWebGL, WebGLTex, Buffer, WebGLProg, WebGLContext, Float, Uniform, Vec3, Bool, Sampler2D, Mat4, Vec2,
        Attribute, BlendingFactor(ONE, SRC_ALPHA), BufferTarget(ELEMENT_ARRAY_BUFFER), Capacity(DEPTH_TEST, BLEND), Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT),
        Mode(TRIANGLES), Shaders(Shaders), TexFilterSpec(MIPMAP), setUniformFloats, setUniformBoolean, enable, disable, blendFunc, drawElements,
        bindBuf, withTexture2D, bindBufAndSetVertexAttr, clear, viewport, getCanvasHeight, getCanvasWidth, requestAnimationFrame, texture2DFor,
        clearColor, makeBuffer, makeBufferFloat, withShaders, runWebGL)
import Data.Matrix (toArray) as M
import Data.Matrix4 (identity, translate, rotate, makePerspective) as M
import Data.Matrix3 (normalFromMat4)
import Data.Vector (toArray, normalize, scale) as V
import Data.Vector3 (vec3, vec3') as V
import Data.ArrayBuffer.Types (Uint16, Float32) as T
import Data.TypedArray (asUint16Array) as T
import KeyEvent (Event, eventGetKeyCode, getElementByIdFloat, getElementByIdBool, onKeyUp, onKeyDown)

type MyBindings =
              (aVertexPosition :: Attribute Vec3, aVertexNormal :: Attribute Vec3,aTextureCoord :: Attribute Vec2,
              uPMatrix :: Uniform Mat4, uMVMatrix:: Uniform Mat4, uNMatrix:: Uniform Mat4, uSampler :: Uniform Sampler2D,
              uUseLighting :: Uniform Bool, uAmbientColor :: Uniform Vec3, uLightingDirection :: Uniform Vec3,
              uDirectionalColor :: Uniform Vec3, uAlpha :: Uniform Float)

shaders :: Shaders (Record MyBindings)
shaders = Shaders

  """
      precision mediump float;

      varying vec2 vTextureCoord;
      varying vec3 vLightWeighting;

      uniform float uAlpha;

      uniform sampler2D uSampler;

      void main(void) {
          vec4 textureColor = texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t));
          gl_FragColor = vec4(textureColor.rgb * vLightWeighting, textureColor.a * uAlpha);
      }
  """

  """
      attribute vec3 aVertexPosition;
      attribute vec3 aVertexNormal;
      attribute vec2 aTextureCoord;

      uniform mat4 uMVMatrix;
      uniform mat4 uPMatrix;
      uniform mat3 uNMatrix;

      uniform vec3 uAmbientColor;

      uniform vec3 uLightingDirection;
      uniform vec3 uDirectionalColor;

      uniform bool uUseLighting;

      varying vec2 vTextureCoord;
      varying vec3 vLightWeighting;

      void main(void) {
          gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
          vTextureCoord = aTextureCoord;

          if (!uUseLighting) {
              vLightWeighting = vec3(1.0, 1.0, 1.0);
          } else {
              vec3 transformedNormal = uNMatrix * aVertexNormal;
              float directionalLightWeighting = max(dot(transformedNormal, uLightingDirection), 0.0);
              vLightWeighting = uAmbientColor + uDirectionalColor * directionalLightWeighting;
          }
      }
  """

cubeV :: Array Number
cubeV = [
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
        -1.0,  1.0, -1.0
      ]

vertexNormals :: Array Number
vertexNormals = [
        -- Front face
         0.0,  0.0,  1.0,
         0.0,  0.0,  1.0,
         0.0,  0.0,  1.0,
         0.0,  0.0,  1.0,

        -- Back face
         0.0,  0.0, -1.0,
         0.0,  0.0, -1.0,
         0.0,  0.0, -1.0,
         0.0,  0.0, -1.0,

        -- Top face
         0.0,  1.0,  0.0,
         0.0,  1.0,  0.0,
         0.0,  1.0,  0.0,
         0.0,  1.0,  0.0,

        -- Bottom face
         0.0, -1.0,  0.0,
         0.0, -1.0,  0.0,
         0.0, -1.0,  0.0,
         0.0, -1.0,  0.0,

        -- Right face
         1.0,  0.0,  0.0,
         1.0,  0.0,  0.0,
         1.0,  0.0,  0.0,
         1.0,  0.0,  0.0,

        -- Left face
        -1.0,  0.0,  0.0,
        -1.0,  0.0,  0.0,
        -1.0,  0.0,  0.0,
        -1.0,  0.0,  0.0
      ]

texCoo :: Array Number
texCoo = [
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

cvi :: Array Int
cvi = [
        0, 1, 2,      0, 2, 3,    -- Front face
        4, 5, 6,      4, 6, 7,    -- Back face
        8, 9, 10,     8, 10, 11,  -- Top face
        12, 13, 14,   12, 14, 15, -- Bottom face
        16, 17, 18,   16, 18, 19, -- Right face
        20, 21, 22,   20, 22, 23  -- Left face
      ]

type State bindings = {
                context :: WebGLContext,
                bindings :: {webGLProgram :: WebGLProg | bindings},

                cubeVertices :: Buffer T.Float32,
                cubeVerticesNormal :: Buffer T.Float32,
                textureCoords :: Buffer T.Float32,
                cubeVertexIndices :: Buffer T.Uint16,
                texture :: WebGLTex,

                lastTime :: Maybe Number,
                xRot :: Number,
                xSpeed :: Number,
                yRot :: Number,
                ySpeed :: Number,
                z :: Number,
                currentlyPressedKeys :: Array Int
              }

main :: Eff (console :: CONSOLE, alert :: Alert, clock :: CLOCK) Unit
main = do
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        log "WebGL started"
        withShaders
            shaders
            (\s -> alert s)
            \ bindings -> do
          cubeVertices <- makeBufferFloat cubeV
          cubeVerticesNormal <- makeBufferFloat vertexNormals

          textureCoords <- makeBufferFloat texCoo
          cubeVertexIndices <- makeBuffer ELEMENT_ARRAY_BUFFER T.asUint16Array cvi
          clearColor 0.0 0.0 0.0 1.0
          enable DEPTH_TEST
          texture2DFor "glass.gif" MIPMAP \texture -> do
            let state = {
                          context : context,
                          bindings : bindings,

                          cubeVertices : cubeVertices,
                          cubeVerticesNormal : cubeVerticesNormal,
                          textureCoords : textureCoords,
                          cubeVertexIndices : cubeVertexIndices,
                          texture : texture,
                          lastTime : Nothing,

                          xRot : 0.0,
                          xSpeed : 1.0,
                          yRot : 0.0,
                          ySpeed : 1.0,
                          z : (-5.0),
                          currentlyPressedKeys : []
                        } :: State MyBindings
            runST do
              stRef <- newSTRef state
              onKeyDown (handleKeyD stRef)
              onKeyUp (handleKeyU stRef)
              tick stRef

tick :: forall h eff. STRef h (State MyBindings) ->  EffWebGL (st :: ST h, console :: CONSOLE, clock :: CLOCK |eff) Unit
tick stRef = do
  drawScene stRef
  handleKeys stRef
  animate stRef
  requestAnimationFrame (tick stRef)

animate ::  forall h eff . STRef h (State MyBindings) -> EffWebGL (st :: ST h, clock :: CLOCK |eff) Unit
animate stRef = do
  s <- readSTRef stRef
  timeNow <- milliseconds
  _ <- case s.lastTime of
    Nothing -> writeSTRef stRef (s {lastTime = Just timeNow})
    Just lastt ->
      let elapsed = timeNow - lastt
      in writeSTRef stRef (s {lastTime = Just timeNow,
                              xRot = s.xRot + s.xSpeed * elapsed / 1000.0,
                              yRot = s.yRot + s.ySpeed * elapsed / 1000.0
                              })
  pure unit

drawScene :: forall h eff . STRef h (State MyBindings) -> EffWebGL (st :: ST h |eff) Unit
drawScene stRef = do
  s <- readSTRef stRef
  canvasWidth <- getCanvasWidth s.context
  canvasHeight <- getCanvasHeight s.context
  viewport 0 0 canvasWidth canvasHeight
  clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

  let pMatrix = M.makePerspective 45.0 (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
  setUniformFloats s.bindings.uPMatrix (M.toArray pMatrix)

  let mvMatrix = M.rotate (degToRad s.yRot) (V.vec3' [0.0, 1.0, 0.0])
                    $ M.rotate (degToRad s.xRot) (V.vec3' [1.0, 0.0, 0.0])
                      $ M.translate  (V.vec3 0.0 0.0 s.z)
                        $ M.identity
  setUniformFloats s.bindings.uMVMatrix (M.toArray mvMatrix)

  let nMatrix = unsafePartial $ fromJust $ normalFromMat4 mvMatrix
  setUniformFloats s.bindings.uNMatrix (M.toArray nMatrix)

  setBlending s
  setLightning s

  bindBufAndSetVertexAttr s.cubeVertices s.bindings.aVertexPosition
  bindBufAndSetVertexAttr s.cubeVerticesNormal s.bindings.aVertexNormal
  bindBufAndSetVertexAttr s.textureCoords s.bindings.aTextureCoord
  withTexture2D s.texture 0 s.bindings.uSampler 0
      ( do
          bindBuf s.cubeVertexIndices
          drawElements TRIANGLES s.cubeVertexIndices.bufferSize)


setBlending :: forall eff. (State MyBindings) -> EffWebGL eff Unit
setBlending s = do
    blending <- getElementByIdBool "blending"
    if blending
        then do
            alpha <- getElementByIdFloat "alpha"
            blendFunc SRC_ALPHA ONE
            enable BLEND
            disable DEPTH_TEST
            setUniformFloats s.bindings.uAlpha [alpha]
        else do
            disable BLEND
            enable DEPTH_TEST


setLightning :: forall eff. (State MyBindings) -> EffWebGL eff Unit
setLightning s = do
  lighting <- getElementByIdBool "lighting"
  setUniformBoolean s.bindings.uUseLighting lighting
  if lighting
    then do
      ar <- getElementByIdFloat "ambientR"
      ag <- getElementByIdFloat "ambientG"
      ab <- getElementByIdFloat "ambientB"
      setUniformFloats s.bindings.uAmbientColor [ar, ag, ab]
      lx <- getElementByIdFloat "lightDirectionX"
      ly <- getElementByIdFloat "lightDirectionY"
      lz <- getElementByIdFloat "lightDirectionZ"
      let v = V.scale (-1.0)
                  $ V.normalize
                    $ V.vec3 lx ly lz
      setUniformFloats s.bindings.uLightingDirection (V.toArray v)
      dr <- getElementByIdFloat "directionalR"
      dg <- getElementByIdFloat "directionalG"
      db <- getElementByIdFloat "directionalB"
      setUniformFloats s.bindings.uDirectionalColor [dr, dg, db]
    else pure unit

-- | Convert from radians to degrees.
radToDeg :: Number -> Number
radToDeg x = x/pi*180.0

-- | Convert from degrees to radians.
degToRad :: Number -> Number
degToRad x = x/180.0*pi

-- * Key handling

handleKeys ::  forall h eff . STRef h (State MyBindings) -> EffWebGL (console :: CONSOLE, st :: ST h |eff) Unit
handleKeys stRef = do
  s <- readSTRef stRef
  if null s.currentlyPressedKeys
    then pure unit
    else
      let z' = case elemIndex 33 s.currentlyPressedKeys of
                  Just _ ->  s.z - 0.05
                  Nothing -> s.z
          z'' = case elemIndex 34 s.currentlyPressedKeys of
                  Just _ ->  z' + 0.05
                  Nothing -> z'
          ySpeed' = case elemIndex 37 s.currentlyPressedKeys of
                  Just _ ->  s.ySpeed - 1.0
                  Nothing -> s.ySpeed
          ySpeed'' = case elemIndex 39 s.currentlyPressedKeys of
                  Just _ ->  ySpeed' + 1.0
                  Nothing -> ySpeed'
          xSpeed' = case elemIndex 38 s.currentlyPressedKeys of
                  Just _ ->  s.xSpeed - 1.0
                  Nothing -> s.xSpeed
          xSpeed'' = case elemIndex 40 s.currentlyPressedKeys of
                  Just _ ->  xSpeed' + 1.0
                  Nothing -> xSpeed'
      in do
        _ <- writeSTRef stRef (s{z=z'',ySpeed=ySpeed'',xSpeed=xSpeed''})
--        log (show s.currentlyPressedKeys)
        pure unit

handleKeyD :: forall h eff. STRef h (State MyBindings) -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
handleKeyD stRef event = do
  log "handleKeyDown"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  let cp = case elemIndex key s.currentlyPressedKeys of
                  Just _ ->  s.currentlyPressedKeys
                  Nothing -> key : s.currentlyPressedKeys
  _ <- writeSTRef stRef (s {currentlyPressedKeys = cp})
--  log (show s.currentlyPressedKeys)
  pure unit

handleKeyU :: forall h eff. STRef h (State MyBindings) -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
handleKeyU stRef event = do
  log "handleKeyUp"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  case elemIndex key s.currentlyPressedKeys of
    Nothing ->  pure unit
    Just _ -> do
      _ <- writeSTRef stRef (s {currentlyPressedKeys = delete key s.currentlyPressedKeys})
      -- log (show s.currentlyPressedKeys)
      pure unit
