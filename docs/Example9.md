## Module Example9

#### `starCount`

``` purescript
starCount :: Int
```

#### `spinStep`

``` purescript
spinStep :: Number
```

#### `MyBindings`

``` purescript
type MyBindings = (aVertexPosition :: Attribute Vec3, aTextureCoord :: Attribute Vec2, uPMatrix :: Uniform Mat4, uMVMatrix :: Uniform Mat4, uSampler :: Uniform Sampler2D, uColor :: Uniform Vec3)
```

#### `shaders`

``` purescript
shaders :: Shaders {  | MyBindings }
```

#### `State`

``` purescript
type State bindings = { context :: WebGLContext, bindings :: { webGLProgram :: WebGLProg | bindings }, starVertices :: Buffer Float32, textureCoords :: Buffer Float32, texture :: WebGLTex, lastTime :: Maybe Number, stars :: Array Star, spin :: Number, tilt :: Number, z :: Number, currentlyPressedKeys :: Array Int, benchCount :: Int, benchTime :: Number }
```

#### `vertices`

``` purescript
vertices :: Array Number
```

#### `texCoo`

``` purescript
texCoo :: Array Number
```

#### `Star`

``` purescript
type Star = { angle :: Number, dist :: Number, rotationSpeed :: Number, r :: Number, g :: Number, b :: Number, twinkleR :: Number, twinkleG :: Number, twinkleB :: Number }
```

Star attributes

#### `starDefault`

``` purescript
starDefault :: Number -> Number -> Star
```

#### `starCreate`

``` purescript
starCreate :: forall eff. Number -> Number -> Eff (random :: RANDOM | eff) Star
```

#### `starRANDOMiseColors`

``` purescript
starRANDOMiseColors :: forall eff. Star -> Eff (random :: RANDOM | eff) Star
```

#### `starAnimate`

``` purescript
starAnimate :: forall eff. Int -> Star -> EffWebGL (random :: RANDOM | eff) Star
```

#### `starDraw`

``` purescript
starDraw :: forall eff. State MyBindings -> Mat4 -> Boolean -> Tuple Star Number -> EffWebGL eff Unit
```

#### `main`

``` purescript
main :: Eff (console :: CONSOLE, alert :: Alert, now :: Now, random :: RANDOM) Unit
```

#### `tick`

``` purescript
tick :: forall h eff. STRef h (State MyBindings) -> EffWebGL (st :: ST h, console :: CONSOLE, now :: Now, random :: RANDOM | eff) Unit
```

#### `unpackMilliseconds`

``` purescript
unpackMilliseconds :: Milliseconds -> Number
```

#### `animate`

``` purescript
animate :: forall h eff. STRef h (State MyBindings) -> EffWebGL (st :: ST h, now :: Now, random :: RANDOM | eff) Unit
```

#### `drawScene`

``` purescript
drawScene :: forall h eff. STRef h (State MyBindings) -> EffWebGL (st :: ST h | eff) Unit
```

#### `drawStar`

``` purescript
drawStar :: forall eff. State MyBindings -> Mat4 -> EffWebGL eff Unit
```

#### `iterateN`

``` purescript
iterateN :: forall a. (a -> a) -> Int -> a -> Array a
```

collects results of repeated function application, up to n times

#### `radToDeg`

``` purescript
radToDeg :: Number -> Number
```

Convert from radians to degrees.

#### `degToRad`

``` purescript
degToRad :: Number -> Number
```

Convert from degrees to radians.

#### `handleKeys`

``` purescript
handleKeys :: forall h eff. STRef h (State MyBindings) -> EffWebGL (console :: CONSOLE, st :: ST h | eff) Unit
```

#### `handleKeyD`

``` purescript
handleKeyD :: forall h eff. STRef h (State MyBindings) -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
```

#### `handleKeyU`

``` purescript
handleKeyU :: forall h eff. STRef h (State MyBindings) -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
```


