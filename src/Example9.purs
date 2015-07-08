module Example9 where

import Prelude
import Control.Monad.Eff.WebGL
import Graphics.WebGL
import Graphics.WebGLRaw
import Graphics.WebGLTexture
import qualified Data.Matrix as M
import qualified Data.Matrix4 as M
import qualified Data.Vector as V
import qualified Data.Vector3 as V
import qualified Data.ArrayBuffer.Types as T
import qualified Data.TypedArray as T
import Control.Monad.Eff.Alert
import Extensions

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad
import Control.Monad.ST
import Control.Monad.Eff.Console
import Data.Tuple
import Data.Foldable (for_)
import Data.Date
import Data.Time
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array
import Math hiding (log)
import Data.Int (toNumber)
import KeyEvent
import Data.Array.Unsafe (unsafeIndex)


starCount   = 50        :: Int
spinStep    = 0.1       :: Number

type MyBindings =
              ( aVertexPosition :: Attribute Vec3
              , aTextureCoord   :: Attribute Vec2
              , uPMatrix        :: Uniform Mat4
              , uMVMatrix       :: Uniform Mat4
              , uSampler        :: Uniform Sampler2D
              , uColor          :: Uniform Vec3
              )

shaders :: Shaders (Object MyBindings)
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
                lastTime :: Maybe Int,

                stars   :: Array Star,
                spin    :: Number,
                tilt    :: Number,
                z       :: Number,
                currentlyPressedKeys :: Array Int
            }

vertices = [
        -1.0, -1.0,  0.0,
         1.0, -1.0,  0.0,
        -1.0,  1.0,  0.0,
         1.0,  1.0,  0.0
        ]


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

starCreate x y =
    starRANDOMiseColors (starDefault x y)

starRANDOMiseColors star = do
    colors <- replicateM 6 random
    return star
        { r             = colors `unsafeIndex` 0
        , g             = colors `unsafeIndex` 1
        , b             = colors `unsafeIndex` 2
        , twinkleR      = colors `unsafeIndex` 3
        , twinkleG      = colors `unsafeIndex` 4
        , twinkleB      = colors `unsafeIndex` 5
        }

starAnimate :: forall eff . Int -> Star -> EffWebGL (random :: RANDOM |eff) Star
starAnimate elapsedTime star = do
    let
        star' = star
            { angle = star.angle + star.rotationSpeed * step
            , dist  = star.dist - 0.01 * step
            }
    if star'.dist < 0.0
        then starRANDOMiseColors star' {dist = star'.dist + 5.0}
        else return star'
  where
    step = (toNumber elapsedTime *  60.0) / 1000.0

starDraw :: forall eff . State MyBindings -> M.Mat4 -> Boolean -> Tuple Star Number -> EffWebGL eff Unit
starDraw s mvMatrix twinkle (Tuple star mySpin) = do
    let
        mv' =
            M.rotate (degToRad $ negate s.tilt) (V.vec3' [1.0, 0.0, 0.0])
                $ M.rotate (degToRad $ negate star.angle) (V.vec3' [0.0, 1.0, 0.0])
                $ M.translate (V.vec3 star.dist 0.0 0.0)
                $ M.rotate (degToRad star.angle) (V.vec3' [0.0, 1.0, 0.0])
                $ mvMatrix
        mv'' =
            M.rotate (degToRad mySpin) (V.vec3' [0.0, 0.0, 1.0]) mv'

    when twinkle $ do
        setUniformFloats s.bindings.uColor [star.twinkleR, star.twinkleG, star.twinkleB]
        drawStar s mv'

    setUniformFloats s.bindings.uColor [star.r, star.g, star.b]
    drawStar s mv''



main :: Eff (console :: CONSOLE, alert :: Alert, now :: Now, random :: RANDOM) Unit
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
                              currentlyPressedKeys : []
                            } :: State MyBindings
                runST do
                  stRef <- newSTRef state
                  onKeyDown (handleKeyD stRef)
                  onKeyUp (handleKeyU stRef)
                  tick (stRef :: STRef _ (State MyBindings))

tick :: forall h eff. STRef h (State MyBindings) ->  EffWebGL (st :: ST h, console :: CONSOLE, now :: Now, random :: RANDOM |eff) Unit
tick stRef = do
  drawScene stRef
  handleKeys stRef
  animate stRef
  requestAnimationFrame (tick stRef)

unpackMilliseconds :: Milliseconds -> Int
unpackMilliseconds (Milliseconds n) = n

animate ::  forall h eff . STRef h (State MyBindings) -> EffWebGL (st :: ST h, now :: Now, random :: RANDOM |eff) Unit
animate stRef = do
  s <- readSTRef stRef
  timeNow <- liftM1 (unpackMilliseconds <<< toEpochMilliseconds) now
  case s.lastTime of
    Nothing -> writeSTRef stRef (s {lastTime = Just timeNow})
    Just lastt ->
      let
        elapsed = timeNow - lastt
        spin'   = s.spin + (spinStep * toNumber (length s.stars))
      in do
        stars' <- mapM (starAnimate elapsed) s.stars
        writeSTRef stRef (s {lastTime = Just timeNow, spin=spin', stars=stars'})
  return unit

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

  let   pMatrix = M.makePerspective 45.0 (toNumber canvasWidth / toNumber canvasHeight) 0.1 100.0
        mvMatrix = M.rotate (degToRad s.tilt) (V.vec3' [1.0, 0.0, 0.0]) $ M.translate (V.vec3 0.0 0.0 s.z) $ M.identity
        ss = zip s.stars (iterateN (+spinStep) (length s.stars) s.spin)

  setUniformFloats s.bindings.uPMatrix (M.toArray pMatrix)
  for_ ss $ starDraw s mvMatrix twinkle


drawStar s mvMatrix = do
  setUniformFloats s.bindings.uMVMatrix (M.toArray mvMatrix)
  drawArr TRIANGLE_STRIP s.starVertices s.bindings.aVertexPosition


-- | collects results of repeated function application, up to n times
iterateN :: forall a . (a -> a) -> Int -> a -> Array a                   -- pfff... I miss Haskell's laziness...
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

handleKeys ::  forall h eff . STRef h (State MyBindings) -> EffWebGL (console :: CONSOLE, st :: ST h |eff) Unit
handleKeys stRef = do
  s <- readSTRef stRef
  if null s.currentlyPressedKeys
    then return unit
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
        writeSTRef stRef (s{z=z'',tilt=tilt''})
        -- log (show s.currentlyPressedKeys)
        return unit

handleKeyD :: forall h eff. STRef h (State MyBindings) -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
handleKeyD stRef event = do
--  log "handleKeyDown"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  let cp = case elemIndex key s.currentlyPressedKeys of
                  Just _ ->  s.currentlyPressedKeys
                  Nothing -> key : s.currentlyPressedKeys
  writeSTRef stRef (s {currentlyPressedKeys = cp})
--  log (show s.currentlyPressedKeys)
  return unit

handleKeyU :: forall h eff. STRef h (State MyBindings) -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
handleKeyU stRef event = do
--  log "handleKeyUp"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  case elemIndex key s.currentlyPressedKeys of
    Nothing ->  return unit
    Just _ -> do
      writeSTRef stRef (s {currentlyPressedKeys = delete key s.currentlyPressedKeys})
      -- log (show s.currentlyPressedKeys)
      return unit
