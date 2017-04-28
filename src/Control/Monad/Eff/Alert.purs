module Control.Monad.Eff.Alert where

import Prelude (Unit)
import Control.Monad.Eff (kind Effect, Eff)

foreign import data Alert :: Effect

foreign import alert :: forall eff. String -> Eff (alert :: Alert | eff) Unit
