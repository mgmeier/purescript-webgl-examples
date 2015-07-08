module Control.Monad.Eff.Alert where

import Prelude
import Control.Monad.Eff

foreign import data Alert :: !

foreign import alert :: forall eff. String -> Eff (alert :: Alert | eff) Unit
