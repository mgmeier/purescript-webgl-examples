module Example6 where

import Prelude (Unit, unit, return, bind, show, (++), (+), (==), (-), (*), (/), ($), (<<<), liftM1, negate)
import Graphics.WebGLAll (EffWebGL, WebGLTex, Buffer, Sampler2D, Uniform, Mat4, Vec2, Attribute, Vec3, WebGLProg, WebGLContext, BufferTarget(ELEMENT_ARRAY_BUFFER), Capacity(DEPTH_TEST), Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT), Mode(TRIANGLES), Shaders(Shaders), TexFilterSpec(MIPMAP, LINEAR, NEAREST), drawElements, bindBuf, withTexture2D, bindBufAndSetVertexAttr, setUniformFloats, clear, viewport, getCanvasHeight, getCanvasWidth, requestAnimationFrame, texture2DFor, enable, clearColor, makeBuffer, makeBufferFloat, withShaders, runWebGL)
import Data.Matrix4 (identity, translate, rotate, makePerspective) as M
import Data.Matrix (toArray) as M
import Data.Vector3 as V3
import Control.Monad.Eff.Alert (Alert, alert)
import Data.ArrayBuffer.Types (Uint16, Float32) as T
import Data.TypedArray (asUint16Array) as T
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, writeSTRef, readSTRef, newSTRef, runST)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Date (Now, now, toEpochMilliseconds)
import Data.Time (Milliseconds(Milliseconds))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.Array (delete, elemIndex, (:), null, (!!))
import Math (pi)
import Data.Int (toNumber)
import KeyEvent (Event, eventGetKeyCode, onKeyUp, onKeyDown)

shaders :: Shaders {aVertexPosition :: Attribute Vec3, aTextureCoord :: Attribute Vec2,
                      uPMatrix :: Uniform Mat4, uMVMatrix:: Uniform Mat4, uSampler :: Uniform Sampler2D}
shaders = Shaders
  """
      precision mediump float;

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

type State = {
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
                textures :: Array WebGLTex,

                lastTime :: Maybe Number,
                xRot :: Number,
                xSpeed :: Number,
                yRot :: Number,
                ySpeed :: Number,
                z :: Number,
                filterInd :: Int,
                currentlyPressedKeys :: Array Int
            }

main :: Eff (console :: CONSOLE, alert :: Alert, now :: Now) Unit
main = do
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        log "WebGL started"
        withShaders shaders
                    (\s -> alert s)
                      \ bindings -> do
          cubeVertices <- makeBufferFloat cubeV
          textureCoords <- makeBufferFloat texCoo
          cubeVertexIndices <- makeBuffer ELEMENT_ARRAY_BUFFER T.asUint16Array cvi
          clearColor 0.0 0.0 0.0 1.0
          enable DEPTH_TEST
          texture2DFor "crate.gif" NEAREST \texture1 ->
            texture2DFor "crate.gif" LINEAR \texture2 ->
              texture2DFor "crate.gif" MIPMAP \texture3 -> do
                let state = {
                              context : context,
                              shaderProgram : bindings.webGLProgram,

                              aVertexPosition : bindings.aVertexPosition,
                              aTextureCoord : bindings.aTextureCoord,
                              uPMatrix : bindings.uPMatrix,
                              uMVMatrix : bindings.uMVMatrix,
                              uSampler : bindings.uSampler,

                              cubeVertices : cubeVertices,
                              textureCoords : textureCoords,
                              cubeVertexIndices : cubeVertexIndices,
                              textures : [texture1,texture2,texture3],
                              lastTime : Nothing,

                              xRot : 0.0,
                              xSpeed : 1.0,
                              yRot : 0.0,
                              ySpeed : 1.0,
                              z : (-5.0),
                              filterInd : 0,
                              currentlyPressedKeys : []
                            }
                runST do
                  stRef <- newSTRef state
                  onKeyDown (handleKeyD stRef)
                  onKeyUp (handleKeyU stRef)
                  tick stRef

tick :: forall h eff. STRef h State ->  EffWebGL (st :: ST h, console :: CONSOLE, now :: Now |eff) Unit
tick stRef = do
  drawScene stRef
  handleKeys stRef
  animate stRef
  requestAnimationFrame (tick stRef)

unpackMilliseconds :: Milliseconds -> Number
unpackMilliseconds (Milliseconds n) = n

animate ::  forall h eff . STRef h State -> EffWebGL (st :: ST h, now :: Now |eff) Unit
animate stRef = do
  s <- readSTRef stRef
  timeNow <- liftM1 (unpackMilliseconds <<< toEpochMilliseconds) now
  case s.lastTime of
    Nothing -> writeSTRef stRef (s {lastTime = Just timeNow})
    Just lastt ->
      let elapsed = timeNow - lastt
      in writeSTRef stRef (s {lastTime = Just timeNow,
                              xRot = s.xRot + s.xSpeed * elapsed / 1000.0,
                              yRot = s.yRot + s.ySpeed * elapsed / 1000.0
                              })
  return unit

drawScene :: forall h eff . STRef h State -> EffWebGL (st :: ST h |eff) Unit
drawScene stRef = do
  s <- readSTRef stRef
  canvasWidth <- getCanvasWidth s.context
  canvasHeight <- getCanvasHeight s.context
  viewport 0 0 canvasWidth canvasHeight
  clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

  let pMatrix = M.makePerspective 45.0 (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
  setUniformFloats s.uPMatrix (M.toArray pMatrix)

  let mvMatrix =
      M.rotate (degToRad s.xRot) (V3.vec3' [1.0, 0.0, 0.0])
        $ M.rotate (degToRad s.yRot) (V3.vec3' [0.0, 1.0, 0.0])
          $ M.translate (V3.vec3 0.0 0.0 s.z)
            $ M.identity
  setUniformFloats s.uMVMatrix (M.toArray mvMatrix)

  bindBufAndSetVertexAttr s.cubeVertices s.aVertexPosition
  bindBufAndSetVertexAttr s.textureCoords s.aTextureCoord

  withTexture2D (fromJust $ s.textures !! s.filterInd) 0 s.uSampler 0

  bindBuf s.cubeVertexIndices
  drawElements TRIANGLES s.cubeVertexIndices.bufferSize



-- | Convert from radians to degrees.
radToDeg :: Number -> Number
radToDeg x = x/pi*180.0

-- | Convert from degrees to radians.
degToRad :: Number -> Number
degToRad x = x/180.0*pi

-- * Key handling

handleKeys ::  forall h eff . STRef h State -> EffWebGL (console :: CONSOLE, st :: ST h |eff) Unit
handleKeys stRef = do
  s <- readSTRef stRef
  if null s.currentlyPressedKeys
    then return unit
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
        writeSTRef stRef (s{z=z'',ySpeed=ySpeed'',xSpeed=xSpeed''})
--        log (show s.currentlyPressedKeys)
        return unit

handleKeyD :: forall h eff. STRef h State -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
handleKeyD stRef event = do
  let key = eventGetKeyCode event
  log ("handleKeyDown: " ++ show key)
  s <- readSTRef stRef
  let f = if key == 70
            then if s.filterInd + 1 == 3
                    then 0
                    else s.filterInd + 1
            else s.filterInd
      cp = case elemIndex key s.currentlyPressedKeys of
                      Just _ ->  s.currentlyPressedKeys
                      Nothing -> key : s.currentlyPressedKeys
  log ("filterInd: " ++ show f)
  writeSTRef stRef (s {currentlyPressedKeys = cp, filterInd = f})
--   log (show s.currentlyPressedKeys)
  return unit

handleKeyU :: forall h eff. STRef h State -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
handleKeyU stRef event = do
  log "handleKeyUp"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  case elemIndex key s.currentlyPressedKeys of
    Nothing ->  return unit
    Just _ -> do
      writeSTRef stRef (s {currentlyPressedKeys = delete key s.currentlyPressedKeys})
      -- log (show s.currentlyPressedKeys)
      return unit
