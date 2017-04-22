module Example9ST where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.ST (ST, STRef, writeSTRef, readSTRef, modifySTRef, newSTRef, runST)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(Tuple), uncurry)
import Data.Foldable (for_)
import System.Clock (CLOCK, milliseconds)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Array (delete, elemIndex, (:), null, reverse, length, zip, (..), unsafeIndex)
import Math (pi)
import Data.Int (toNumber)
import Partial.Unsafe (unsafePartial)

import Control.Monad.Eff.WebGL (EffWebGL)
import Graphics.WebGL (Buffer, WebGLProg, WebGLContext, Vec3, Uniform, Sampler2D, Mat4, Vec2, Attribute, BlendingFactor(ONE, SRC_ALPHA),
    Capacity(BLEND), Mask(DEPTH_BUFFER_BIT, COLOR_BUFFER_BIT), Mode(TRIANGLE_STRIP), Shaders(Shaders), drawArr, setUniformFloats,
    bindBufAndSetVertexAttr, enable, blendFunc, clear, viewport, getCanvasHeight, getCanvasWidth, requestAnimationFrame, clearColor,
    makeBufferFloat, withShaders, runWebGL)
import Graphics.WebGLTexture (WebGLTex, TexFilterSpec(MIPMAP), withTexture2D, texture2DFor)
import Data.Matrix (toArray) as M
import Data.Matrix4 (makePerspective) as M
import Data.ST.Matrix (STMat(STMat), unsafeFreeze, cloneSTMat) as M
import Data.ST.Matrix4 (STMat4, rotateST, translateST, identityST) as M
import Data.Vector3 as V
import Data.ArrayBuffer.Types as T
import Control.Monad.Eff.Alert (Alert, alert)
import Extensions (mapM, replicateM)

import KeyEvent (Event, eventGetKeyCode, getElementByIdBool, onKeyUp, onKeyDown)

starCount :: Int
starCount   = 50
spinStep :: Number
spinStep    = 0.1

type MyBindings =
              ( aVertexPosition :: Attribute Vec3
              , aTextureCoord   :: Attribute Vec2
              , uPMatrix        :: Uniform Mat4
              , uMVMatrix       :: Uniform Mat4
              , uSampler        :: Uniform Sampler2D
              , uColor          :: Uniform Vec3
              )

shaders :: Shaders (Record MyBindings)
shaders = Shaders

  """
        precision mediump float;

        varying vec2 vTextureCoord;

        uniform sampler2D uSampler;
        uniform vec3 uColor;

        void main(void) {
            vec4 textureColor = texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t));
            gl_FragColor = textureColor * vec4(uColor, 1.0);
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

type State bindings = {
                context :: WebGLContext,
                bindings :: {webGLProgram :: WebGLProg | bindings},

                starVertices :: Buffer T.Float32,
                textureCoords :: Buffer T.Float32,
                texture :: WebGLTex,
                lastTime :: Maybe Number,

                stars   :: Array Star,
                spin    :: Number,
                tilt    :: Number,
                z       :: Number,
                currentlyPressedKeys :: Array Int,

                benchCount :: Int,
                benchTime :: Number
            }

vertices :: Array Number
vertices = [
        -1.0, -1.0,  0.0,
         1.0, -1.0,  0.0,
        -1.0,  1.0,  0.0,
         1.0,  1.0,  0.0
        ]

texCoo :: Array Number
texCoo = [
        0.0, 0.0,
        1.0, 0.0,
        0.0, 1.0,
        1.0, 1.0
        ]


-- | Star attributes
type Star =
    { angle             :: Number
    , dist              :: Number
    , rotationSpeed     :: Number
    , r                 :: Number
    , g                 :: Number
    , b                 :: Number
    , twinkleR          :: Number
    , twinkleG          :: Number
    , twinkleB          :: Number
    }


-- Star methods
starDefault :: Number -> Number -> Star
starDefault startDist rotSpeed =
    { angle         : 0.0
    , dist          : startDist
    , rotationSpeed : rotSpeed
    , r             : 0.0
    , g             : 0.0
    , b             : 0.0
    , twinkleR      : 0.0
    , twinkleG      : 0.0
    , twinkleB      : 0.0
    }

starCreate :: forall eff . Number -> Number -> Eff (random :: RANDOM| eff) Star
starCreate x y =
    starRandomiseColors (starDefault x y)

starRandomiseColors :: forall eff . Star -> Eff (random :: RANDOM| eff) Star
starRandomiseColors star = do
    colors <- replicateM 6 random
    pure star
        { r             = colors `index` 0
        , g             = colors `index` 1
        , b             = colors `index` 2
        , twinkleR      = colors `index` 3
        , twinkleG      = colors `index` 4
        , twinkleB      = colors `index` 5
        }
  where
    index a b = unsafePartial $ unsafeIndex a b

starAnimate :: forall eff . Int -> Star -> EffWebGL (random :: RANDOM |eff) Star
starAnimate elapsedTime star = do
    let
        star' = star
            { angle = star.angle + star.rotationSpeed * step
            , dist  = star.dist - 0.01 * step
            }
    if star'.dist < 0.0
        then starRandomiseColors star' {dist = star'.dist + 5.0}
        else pure star'
  where
    step = (toNumber elapsedTime *  60.0) / 1000.0

starDraw :: forall h eff . State MyBindings -> Boolean -> M.STMat4 h -> Tuple Star Number -> EffWebGL (st :: ST h |eff) Unit
starDraw s twinkle mvMatrix (Tuple star mySpin) = do
    mv <- M.cloneSTMat mvMatrix
    M.rotateST (degToRad star.angle) (V.vec3' [0.1, 1.0, 0.0]) mv
    M.translateST (V.vec3 star.dist 0.0 0.0) mv
    M.rotateST (degToRad $ negate star.angle) (V.vec3' [0.0, 1.0, 0.0]) mv
    M.rotateST (degToRad $ negate s.tilt) (V.vec3' [1.0, 0.0, 0.0]) mv

    when twinkle $ do
        setUniformFloats s.bindings.uColor [star.twinkleR, star.twinkleG, star.twinkleB]
        drawStar s mv

    M.rotateST (degToRad mySpin) (V.vec3' [0.0, 0.0, 1.0]) mv
    setUniformFloats s.bindings.uColor [star.r, star.g, star.b]
    drawStar s mv


main :: Eff (console :: CONSOLE, alert :: Alert, clock :: CLOCK, random :: RANDOM) Unit
main =
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        log "WebGL started"
        withShaders
            shaders
            (\s -> alert s)
            \ bindings -> do
              vs <- makeBufferFloat vertices
              textureCoords <- makeBufferFloat texCoo
              let starParams i = Tuple ((toNumber i / toNumber starCount) * 5.0) (toNumber i / toNumber starCount)
              ss <- mapM (uncurry starCreate <<< starParams) (0 .. (starCount-1))
              clearColor 0.0 0.0 0.0 1.0
              texture2DFor "star.gif" MIPMAP \texture -> do
                let state = {
                              context : context,
                              bindings : bindings,

                              starVertices : vs,
                              textureCoords : textureCoords,
                              texture : texture,
                              lastTime : Nothing,

                              stars : ss,
                              spin  : 0.0,
                              tilt  : 90.0,
                              z     : (-15.0),
                              currentlyPressedKeys : [],

                              benchTime : 0.0,
                              benchCount : 0
                            } :: State MyBindings
                runST do
                  stRef <- newSTRef state
                  onKeyDown (handleKeyD stRef)
                  onKeyUp (handleKeyU stRef)
                  tick stRef

tick :: forall h eff. STRef h (State MyBindings) ->  EffWebGL (st :: ST h, console :: CONSOLE, clock :: CLOCK, random :: RANDOM |eff) Unit
tick stRef = do
  timeBefore <- milliseconds
  drawScene stRef
  handleKeys stRef
  animate stRef
  timeAfter <- milliseconds
  _ <- modifySTRef stRef \s -> s {benchTime = s.benchTime + (timeAfter - timeBefore)}
  state <- readSTRef stRef
  if state.benchCount < 1000
    then do
            _ <- modifySTRef stRef (\s -> s {benchCount = s.benchCount + 1})
            requestAnimationFrame (tick stRef)
            pure unit
    else if state.benchCount == 1000
        then do
                log ("Benchmark 1000 cycles time in milliseconds: " <> show state.benchTime)
                _ <- modifySTRef stRef (\s -> s {benchCount = s.benchCount + 1})
                pure unit
        else pure unit

unpackMilliseconds :: Milliseconds -> Number
unpackMilliseconds (Milliseconds n) = n

animate ::  forall h eff . STRef h (State MyBindings) -> EffWebGL (st :: ST h, clock :: CLOCK, random :: RANDOM |eff) Unit
animate stRef = do
  s <- readSTRef stRef
  timeNow <- milliseconds
  _ <- case s.lastTime of
    Nothing -> writeSTRef stRef (s {lastTime = Just timeNow})
    Just lastt ->
      let
        elapsed = timeNow - lastt
        spin'   = s.spin + (spinStep * toNumber (length s.stars))
      in do
        stars' <- mapM (starAnimate s.benchCount) s.stars
        writeSTRef stRef (s {lastTime = Just timeNow, spin=spin', stars=stars'})
  pure unit

drawScene :: forall h eff . STRef h (State MyBindings) -> EffWebGL (st :: ST h |eff) Unit
drawScene stRef = do
  s <- readSTRef stRef
  canvasWidth <- getCanvasWidth s.context
  canvasHeight <- getCanvasHeight s.context
  twinkle <- getElementByIdBool "twinkle"

  viewport 0 0 canvasWidth canvasHeight
  clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]
  blendFunc SRC_ALPHA ONE
  enable BLEND
  bindBufAndSetVertexAttr s.starVertices s.bindings.aVertexPosition
  bindBufAndSetVertexAttr s.textureCoords s.bindings.aTextureCoord
  withTexture2D s.texture 0 s.bindings.uSampler 0
    (do
          let
            pMatrix = M.makePerspective 45.0 (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
            ss = zip s.stars (iterateN (_+spinStep) (length s.stars) s.spin)

          setUniformFloats s.bindings.uPMatrix (M.toArray pMatrix)
          mvMatrix <- initialMVMatrix s.tilt s.z
          for_ ss $ starDraw s twinkle mvMatrix)


initialMVMatrix :: forall h r. Number -> Number -> Eff (st :: ST h | r) (M.STMat4 h)
initialMVMatrix tilt zoom = do
    m <- M.identityST
    M.translateST (V.vec3' [0.0, 0.0, zoom]) m
    M.rotateST (degToRad tilt) V.i3 m
    pure m

drawStar :: forall eff h. State MyBindings -> M.STMat4 h -> EffWebGL eff Unit
drawStar s (M.STMat mvMatrix) = do
  setUniformFloats s.bindings.uMVMatrix (M.unsafeFreeze mvMatrix)
  drawArr TRIANGLE_STRIP s.starVertices s.bindings.aVertexPosition


-- | collects results of repeated function application, up to n times
iterateN :: forall a . (a -> a) -> Int -> a -> Array a
iterateN f = iterate' []
  where
    iterate' res 0 _ = reverse res
    iterate' res n x = iterate' (x:res) (n-1) (f x)


-- | Convert from radians to degrees.
radToDeg :: Number -> Number
radToDeg x = x/pi*180.0

-- | Convert from degrees to radians.
degToRad :: Number -> Number
degToRad x = x/180.0*pi


-- * Key handling

-- * Key handling

handleKeys ::  forall h eff . STRef h (State MyBindings) -> EffWebGL (console :: CONSOLE, st :: ST h |eff) Unit
handleKeys stRef = do
  s <- readSTRef stRef
  if null s.currentlyPressedKeys
    then pure unit
    else
      let z' = case elemIndex 33 s.currentlyPressedKeys of
                  Just _ -> s.z - 0.1
                  Nothing -> s.z
          z'' = case elemIndex 34 s.currentlyPressedKeys of
                  Just _ -> z' + 0.1
                  Nothing -> z'
          tilt' = case elemIndex 38 s.currentlyPressedKeys of
                  Just _ -> s.tilt - 2.0
                  Nothing -> s.tilt
          tilt'' = case elemIndex 40 s.currentlyPressedKeys of
                  Just _ -> tilt' + 2.0
                  Nothing -> tilt'
      in do
        _ <- writeSTRef stRef (s{z=z'',tilt=tilt''})
        -- log (show s.currentlyPressedKeys)
        pure unit

handleKeyD :: forall h eff. STRef h (State MyBindings) -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
handleKeyD stRef event = do
--  log "handleKeyDown"
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
--  log "handleKeyUp"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  case elemIndex key s.currentlyPressedKeys of
    Nothing ->  pure unit
    Just _ -> do
      _ <- writeSTRef stRef (s {currentlyPressedKeys = delete key s.currentlyPressedKeys})
      -- log (show s.currentlyPressedKeys)
      pure unit
