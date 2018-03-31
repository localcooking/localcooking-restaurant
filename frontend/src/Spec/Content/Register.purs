module Spec.Content.Register where

import Types.Env (env)

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (mkEffFn1)

import Thermite as T
import React as R
import React.DOM as R
import React.ReCaptcha (reCaptcha)

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography

import Unsafe.Coerce (unsafeCoerce)


type State = Unit

initialState :: State
initialState = unit

data Action
  = GotReCaptchaVerify R.Event
  | ReCaptchaLoaded


type Effects eff =
  ( console :: CONSOLE
  | eff)


spec :: forall eff. T.Spec (Effects eff) State Unit Action
spec = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotReCaptchaVerify e -> do
        liftEff $ log $ unsafeCoerce e
      ReCaptchaLoaded -> do
        liftEff $ log "Loaded ReCaptcha"

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: Typography.headline
        , align: Typography.center
        , color: Typography.primary
        } [R.text "Register"]
      , reCaptcha
        { sitekey: env.googleReCaptchaSiteKey
        , verifyCallback: mkEffFn1 (dispatch <<< GotReCaptchaVerify)
        , onloadCallback: dispatch ReCaptchaLoaded
        }
      ]


register :: R.ReactElement
register =
  let {spec: reactSpec, dispatcher} = T.createReactSpec spec initialState
  in  R.createElement (R.createClass reactSpec) unit []
