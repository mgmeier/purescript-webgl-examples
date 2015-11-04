## Module Control.Monad.Eff.Alert

#### `Alert`

``` purescript
data Alert :: !
```

#### `alert`

``` purescript
alert :: forall eff. String -> Eff (alert :: Alert | eff) Unit
```


