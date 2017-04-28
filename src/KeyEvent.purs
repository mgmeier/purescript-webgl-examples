module KeyEvent where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (EffWebGL, WebGl)
foreign import data Event :: Type

foreign import onKeyDown ::  forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit)
    -> Eff (webgl :: WebGl | eff) Unit

foreign import onKeyUp ::  forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit)
    -> Eff (webgl :: WebGl | eff) Unit

foreign import eventGetKeyCode :: Event -> Int

foreign import getElementByIdFloat :: forall eff. String -> (EffWebGL eff Number)

foreign import getElementByIdBool :: forall eff. String -> (EffWebGL eff Boolean)
