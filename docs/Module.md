# Module Documentation

## Module Control.Monad.Eff.Alert

### Types


    data Alert :: !


### Values


    alert :: forall eff. String -> Eff (alert :: Alert | eff) Unit