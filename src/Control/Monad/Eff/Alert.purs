module Control.Monad.Eff.Alert where

import Prelude (Unit)
import Control.Monad.Eff (Eff)

foreign import data Alert :: !

foreign import alert :: forall eff. String -> Eff (alert :: Alert | eff) Unit
