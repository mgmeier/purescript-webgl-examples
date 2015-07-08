module KeyEvent where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.WebGL

foreign import data Event :: *

foreign import onKeyDown ::  forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit)
    -> Eff (webgl :: WebGl | eff) Unit

foreign import onKeyUp ::  forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit)
    -> Eff (webgl :: WebGl | eff) Unit

foreign import eventGetKeyCode :: Event -> Int

foreign import getElementByIdFloat :: forall eff. String -> (EffWebGL eff Number)

foreign import getElementByIdBool :: forall eff. String -> (EffWebGL eff Boolean)
