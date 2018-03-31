module React.ReCaptcha where

import Prelude (Unit)
import React (ReactClass, Event, ReactElement, createElement)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1)


foreign import reCaptchaImpl :: forall props. ReactClass props


type ReCaptchaProps eff =
  { sitekey :: String
  , verifyCallback :: EffFn1 eff Event Unit
  , onloadCallback :: Eff eff Unit
  }


reCaptcha :: forall eff
           . ReCaptchaProps eff -> ReactElement
reCaptcha ps = createElement reCaptchaImpl ps []
