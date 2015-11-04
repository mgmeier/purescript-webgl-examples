## Module Example3

#### `shaders`

``` purescript
shaders :: Shaders { aVertexPosition :: Attribute Vec3, aVertexColor :: Attribute Vec3, uPMatrix :: Uniform Mat4, uMVMatrix :: Uniform Mat4 }
```

#### `State`

``` purescript
type State = { context :: WebGLContext, shaderProgram :: WebGLProg, aVertexPosition :: Attribute Vec3, aVertexColor :: Attribute Vec3, uPMatrix :: Uniform Mat4, uMVMatrix :: Uniform Mat4, buf1 :: Buffer Float32, buf1Colors :: Buffer Float32, buf2 :: Buffer Float32, buf2Colors :: Buffer Float32, lastTime :: Maybe Number, rTri :: Number, rSquare :: Number }
```

#### `main`

``` purescript
main :: forall eff. Eff (console :: CONSOLE, alert :: Alert, now :: Now) Unit
```

#### `tick`

``` purescript
tick :: forall eff. State -> EffWebGL (now :: Now | eff) Unit
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


