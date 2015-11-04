## Module KeyEvent

#### `Event`

``` purescript
data Event :: *
```

#### `onKeyDown`

``` purescript
onKeyDown :: forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit) -> Eff (webgl :: WebGl | eff) Unit
```

#### `onKeyUp`

``` purescript
onKeyUp :: forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit) -> Eff (webgl :: WebGl | eff) Unit
```

#### `eventGetKeyCode`

``` purescript
eventGetKeyCode :: Event -> Int
```

#### `getElementByIdFloat`

``` purescript
getElementByIdFloat :: forall eff. String -> EffWebGL eff Number
```

#### `getElementByIdBool`

``` purescript
getElementByIdBool :: forall eff. String -> EffWebGL eff Boolean
```


