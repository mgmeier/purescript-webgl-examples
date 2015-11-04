## Module Example6

#### `shaders`

``` purescript
shaders :: Shaders { aVertexPosition :: Attribute Vec3, aTextureCoord :: Attribute Vec2, uPMatrix :: Uniform Mat4, uMVMatrix :: Uniform Mat4, uSampler :: Uniform Sampler2D }
```

#### `State`

``` purescript
type State = { context :: WebGLContext, shaderProgram :: WebGLProg, aVertexPosition :: Attribute Vec3, aTextureCoord :: Attribute Vec2, uPMatrix :: Uniform Mat4, uMVMatrix :: Uniform Mat4, uSampler :: Uniform Sampler2D, cubeVertices :: Buffer Float32, textureCoords :: Buffer Float32, cubeVertexIndices :: Buffer Uint16, textures :: Array WebGLTex, lastTime :: Maybe Number, xRot :: Number, xSpeed :: Number, yRot :: Number, ySpeed :: Number, z :: Number, filterInd :: Int, currentlyPressedKeys :: Array Int }
```

#### `main`

``` purescript
main :: Eff (console :: CONSOLE, alert :: Alert, now :: Now) Unit
```

#### `tick`

``` purescript
tick :: forall h eff. STRef h State -> EffWebGL (st :: ST h, console :: CONSOLE, now :: Now | eff) Unit
```

#### `unpackMilliseconds`

``` purescript
unpackMilliseconds :: Milliseconds -> Number
```

#### `animate`

``` purescript
animate :: forall h eff. STRef h State -> EffWebGL (st :: ST h, now :: Now | eff) Unit
```

#### `drawScene`

``` purescript
drawScene :: forall h eff. STRef h State -> EffWebGL (st :: ST h | eff) Unit
```

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
handleKeys :: forall h eff. STRef h State -> EffWebGL (console :: CONSOLE, st :: ST h | eff) Unit
```

#### `handleKeyD`

``` purescript
handleKeyD :: forall h eff. STRef h State -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
```

#### `handleKeyU`

``` purescript
handleKeyU :: forall h eff. STRef h State -> Event -> Eff (st :: ST h, console :: CONSOLE | eff) Unit
```


