module DOM.HTML.Window.Extra (onPopState, queryParams, removeQueryParam) where

import Prelude
import Data.Foreign (Foreign)
import Data.StrMap (StrMap)
import DOM (DOM)
import DOM.HTML.Types (Window, Location)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, mkEffFn1, runEffFn2)


foreign import onPopStateImpl :: forall eff. EffFn2 eff (EffFn1 eff Foreign Unit) Window Unit

onPopState :: forall eff
            . (Foreign -> Eff (dom :: DOM | eff) Unit)
           -> Window
           -> Eff (dom :: DOM | eff) Unit
onPopState f w = runEffFn2 onPopStateImpl (mkEffFn1 f) w


foreign import queryParams :: Location -> StrMap String

foreign import removeQueryParamImpl :: forall eff. EffFn2 (dom :: DOM | eff) Location String Unit

removeQueryParam :: forall eff. Location -> String -> Eff (dom :: DOM | eff) Unit
removeQueryParam = runEffFn2 removeQueryParamImpl
