-- needs psc >= 0.6.6
-- need to start chrome with --allow-file-access-from-files to be able to load local files
-- Example 9: Moving Objects (Open with index9.html)
module Main where

import Control.Monad.Eff.WebGL
import Graphics.WebGL
import Graphics.WebGLRaw
import Graphics.WebGLTexture
import qualified Data.Matrix as M
import qualified Data.Matrix4 as M
import qualified Data.ST.Matrix as M
import qualified Data.ST.Matrix4 as M
import qualified Data.Vector as V
import qualified Data.Vector3 as V
import qualified Data.TypedArray.Types as T
import qualified Data.TypedArray as T
import Control.Monad.Eff.Alert
import Extensions (mapM)

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad
import Control.Monad.ST
import Debug.Trace
import Data.Tuple
import Data.Traversable (for)
import Data.Date
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array
import Data.Array.ST
import Data.Array.ExtendedRepl
import Math
import Prelude.Unsafe (unsafeIndex)


starCount   = 50        :: Number
spinStep    = 0.1       :: Number

data MyState

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

                tilt    :: Number,
                z       :: Number,
                currentlyPressedKeys :: [Number],

                animate :: STRef MyState Animate

            }

type Animate = {
                lastTime :: Maybe Number,
                stars   :: [Star],
                spin    :: Number
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
    { angle         : 0
    , dist          : startDist
    , rotationSpeed : rotSpeed
    , r             : 0
    , g             : 0
    , b             : 0
    , twinkleR      : 0
    , twinkleG      : 0
    , twinkleB      : 0
    }

starCreate x y =
    starRandomiseColors (starDefault x y)

starRandomiseColors star = do
    colors <- replicateM 6 random
    return star
        { r             = colors `unsafeIndex` 0
        , g             = colors `unsafeIndex` 1
        , b             = colors `unsafeIndex` 2
        , twinkleR      = colors `unsafeIndex` 3
        , twinkleG      = colors `unsafeIndex` 4
        , twinkleB      = colors `unsafeIndex` 5
        }

starAnimate :: forall eff . Number -> Star -> EffWebGL (random :: Random |eff) Star
starAnimate elapsedTime star = do
    let
        star' = star
            { angle = star.angle + star.rotationSpeed * step
            , dist  = star.dist - 0.01 * step
            }
    if star'.dist < 0.0
        then starRandomiseColors star' {dist = star'.dist + 5.0}
        else return star'
  where
    step = (elapsedTime *  60) / 1000

starDraw :: forall h eff . State MyBindings -> Boolean -> M.STMat4 h -> Tuple Star Number -> EffWebGL (st :: ST h |eff) Unit
starDraw s twinkle mvMatrix (Tuple star mySpin) = do
    mv <- M.cloneSTMat mvMatrix
    M.rotateST (degToRad star.angle) (V.j) mv
    M.translateST (V.vec3' [star.dist, 0, 0]) mv
    M.rotateST (degToRad $ negate star.angle) (V.j) mv
    M.rotateST (degToRad $ negate s.tilt) (V.i) mv

    if twinkle
      then do
        setUniformFloats s.bindings.uColor [star.twinkleR, star.twinkleG, star.twinkleB]
        drawStar s mv
      else return unit

    M.rotateST (degToRad mySpin) (V.k) mv
    setUniformFloats s.bindings.uColor [star.r, star.g, star.b]
    drawStar s mv

drawStar s (M.STMat mvMatrix) = do
  setUniformFloats s.bindings.uMVMatrix (M.unsafeFreeze mvMatrix)
  drawArr TRIANGLE_STRIP s.starVertices s.bindings.aVertexPosition

main :: Eff (st :: ST MyState, trace :: Trace, alert :: Alert, now :: Now, random :: Random) Unit
main = do
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        trace "WebGL started-"
        withShaders
            shaders
            (\s -> alert s)
            \ bindings -> do
              vs <- makeBufferSimple vertices
              textureCoords <- makeBufferSimple texCoo
              let starParams i = Tuple ((i / starCount) * 5.0) (i / starCount)
              ss <- mapM (uncurry starCreate <<< starParams) (0 .. (starCount-1))
              clearColor 0.0 0.0 0.0 1.0
              texture2DFor "star.gif" MIPMAP \texture -> do

                let animate = {
                            lastTime : Nothing,
                            stars : ss,
                            spin  : 0.0
                            }
                aniRef <- newSTRef animate
                let state = {
                              context : context,
                              bindings : bindings,

                              starVertices : vs,
                              textureCoords : textureCoords,
                              texture : texture,
                              animate : aniRef,
                              tilt  : 90.0,
                              z     : (-15.0),
                              currentlyPressedKeys : []
                            }
                stRef <- newSTRef state
                onKeyDown (handleKeyD stRef)
                onKeyUp (handleKeyU stRef)
                tick stRef

tick :: forall eff. STRef MyState (State MyBindings) ->  EffWebGL (st :: ST MyState, trace :: Trace, now :: Now, random :: Random |eff) Unit
tick stRef = do
  drawScene stRef
  handleKeys stRef
  animate stRef
  requestAnimationFrame (tick stRef)

animate ::  forall eff . STRef MyState (State MyBindings) -> EffWebGL (st :: ST MyState, now :: Now, random :: Random |eff) Unit
animate stRef = do
  s <- readSTRef stRef
  ani <- readSTRef s.animate
  timeNow <- liftM1 toEpochMilliseconds now
  case ani.lastTime of
    Nothing -> writeSTRef s.animate (ani {lastTime = Just timeNow})
    Just lastt ->
      let
        elapsed = timeNow - lastt
        spin'   = ani.spin + (spinStep * length ani.stars)
      in do
        stars' <- mapM (starAnimate elapsed) ani.stars
        writeSTRef s.animate (ani {lastTime = Just timeNow, spin=spin', stars=stars'})
  return unit

drawScene :: forall eff . STRef MyState (State MyBindings) -> EffWebGL (st :: ST MyState |eff) Unit
drawScene stRef = do
  s <- readSTRef stRef
  canvasWidth <- getCanvasWidth s.context
  canvasHeight <- getCanvasHeight s.context
  twinkle <- getElementByIdBool "twinkle"

  viewport 0 0 canvasWidth canvasHeight
  clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]
  blendFunc SRC_ALPHA ONE
  enable BLEND
  bindPointBuf s.starVertices s.bindings.aVertexPosition
  bindPointBuf s.textureCoords s.bindings.aTextureCoord
  withTexture2D s.texture 0 s.bindings.uSampler 0

  ani <- readSTRef s.animate
  r <- iterateN (+spinStep) (length ani.stars) ani.spin
  let ss = zip ani.stars r
  let pMatrix = M.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
  setUniformFloats s.bindings.uPMatrix (M.toArray pMatrix)

  mvMatrix <- initialMVMatrix s.tilt s.z
  foreachE ss (starDraw s twinkle mvMatrix)

initialMVMatrix :: forall h r. Number -> Number -> Eff (st :: ST h | r) (M.STMat4 h)
initialMVMatrix tilt zoom = do
    m <- M.identityST
    M.translateST (V.vec3' [0.0, 0.0, zoom]) m
    M.rotateST (degToRad tilt) V.i m
    return m




-- | collects results of repeated function application, up to n times
iterateN :: forall a h r. (a -> a) -> Number -> a -> Eff (st :: ST h | r) [a]                   -- pfff... I miss Haskell's laziness...
iterateN f n a = do
  e <- emptySTArray
  iterate' e n a
    where
      iterate' res 0 _ = do
        res' <- freeze res
        return (reverse res')
      iterate' res n x = do
        pushSTArray res x
        iterate' res (n-1) (f x)


foreign import getElementByIdFloat
"""
  function getElementByIdFloat(targ_id) {
      return function () {
        return parseFloat(document.getElementById(targ_id).value);
      };
    }
""" :: forall eff. String -> (EffWebGL eff Number)

foreign import getElementByIdBool
"""
  function getElementByIdBool(targ_id) {
      return function () {
        var elt = document.getElementById(targ_id);
        return elt && elt.checked;
      };
    }
""" :: forall eff. String -> (EffWebGL eff Boolean)

-- | Convert from radians to degrees.
radToDeg :: Number -> Number
radToDeg x = x/pi*180

-- | Convert from degrees to radians.
degToRad :: Number -> Number
degToRad x = x/180*pi


-- * Key handling

handleKeys ::  forall h eff . STRef h (State MyBindings) -> EffWebGL (trace :: Trace, st :: ST h |eff) Unit
handleKeys stRef = do
  s <- readSTRef stRef
  if null s.currentlyPressedKeys
    then return unit
    else
      let z' = if elemIndex 33 s.currentlyPressedKeys /= -1
                  then s.z - 0.1
                  else s.z
          z'' = if elemIndex 34 s.currentlyPressedKeys /= -1
                  then z' + 0.1
                  else z'
          tilt' = if elemIndex 38 s.currentlyPressedKeys /= -1
                  then s.tilt - 2
                  else s.tilt
          tilt'' = if elemIndex 40 s.currentlyPressedKeys /= -1
                  then tilt' + 2
                  else tilt'
      in do
        writeSTRef stRef (s{z=z'',tilt=tilt''})
        -- trace (show s.currentlyPressedKeys)
        return unit

handleKeyD :: forall h eff. STRef h (State MyBindings) -> Event -> Eff (st :: ST h, trace :: Trace | eff) Unit
handleKeyD stRef event = do
  -- trace "handleKeyDown"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  let cp = if elemIndex key s.currentlyPressedKeys /= -1
              then s.currentlyPressedKeys
              else key : s.currentlyPressedKeys
  writeSTRef stRef (s {currentlyPressedKeys = cp})
  -- trace (show s.currentlyPressedKeys)
  return unit

handleKeyU :: forall h eff. STRef h (State MyBindings) -> Event -> Eff (st :: ST h, trace :: Trace | eff) Unit
handleKeyU stRef event = do
  -- trace "handleKeyUp"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  if elemIndex key s.currentlyPressedKeys == -1
    then return unit
    else do
      writeSTRef stRef (s {currentlyPressedKeys = delete key s.currentlyPressedKeys})
       -- trace (show s.currentlyPressedKeys)
      return unit

foreign import data Event :: *

foreign import onKeyDown
"""
        function onKeyDown(handleKeyDown) {
          return function() {
            document.onkeydown = function(event) {handleKeyDown(event)()};
            };}
""" ::  forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit)
    -> Eff (webgl :: WebGl | eff) Unit

foreign import onKeyUp
"""
        function onKeyUp(handleKeyUp) {
          return function() {
            document.onkeyup = function(event) {handleKeyUp(event)()};
            };}
""" ::  forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit)
    -> Eff (webgl :: WebGl | eff) Unit

foreign import eventGetKeyCode
"""
  function eventGetKeyCode (event) {
      return (event.keyCode);
      }
""" :: Event -> Number
