## Module Example5

#### `shaders`

``` purescript
shaders :: Shaders { aVertexPosition :: Attribute Vec3, aTextureCoord :: Attribute Vec2, uPMatrix :: Uniform Mat4, uMVMatrix :: Uniform Mat4, uSampler :: Uniform Sampler2D }
```

#### `State`

``` purescript
type State = { context :: WebGLContext, shaderProgram :: WebGLProg, aVertexPosition :: Attribute Vec3, aTextureCoord :: Attribute Vec2, uPMatrix :: Uniform Mat4, uMVMatrix :: Uniform Mat4, uSampler :: Uniform Sampler2D, cubeVertices :: Buffer Float32, textureCoords :: Buffer Float32, cubeVertexIndices :: Buffer Uint16, texture :: WebGLTex, lastTime :: Maybe Number, rot :: Number }
```

#### `main`

``` purescript
main :: Eff (console :: CONSOLE, alert :: Alert, now :: Now) Unit
```

#### `tick`

``` purescript
tick :: forall eff. State -> EffWebGL (console :: CONSOLE, now :: Now | eff) Unit
```

#### `unpackMilliseconds`

``` purescript
unpackMilliseconds :: Milliseconds -> Number
```

#### `animate`

``` purescript
animate :: forall eff. State -> EffWebGL (now :: Now | eff) State
```

#### `drawScene`

``` purescript
drawScene :: forall eff. State -> EffWebGL (now :: Now | eff) Unit
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


