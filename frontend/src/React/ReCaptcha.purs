module React.ReCaptcha
  ( reCaptcha, ReCaptchaResponse
  ) where

import Prelude
import React (ReactClass, ReactElement, createElement)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Generic (class Generic, gEq, gShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1)


foreign import reCaptchaImpl :: forall props. ReactClass props

newtype ReCaptchaResponse = ReCaptchaResponse Int

derive instance genericReCaptchaResponse :: Generic ReCaptchaResponse

instance eqReCaptchaResponse :: Eq ReCaptchaResponse where
  eq = gEq

instance showReCaptchaResponse :: Show ReCaptchaResponse where
  show = gShow

instance encodeJsonReCaptchaResponse :: EncodeJson ReCaptchaResponse where
  encodeJson (ReCaptchaResponse x) = encodeJson x

instance decodeJsonReCaptchaResponse :: DecodeJson ReCaptchaResponse where
  decodeJson json = ReCaptchaResponse <$> decodeJson json


type ReCaptchaProps eff =
  { sitekey :: String
  , verifyCallback :: EffFn1 eff ReCaptchaResponse Unit
  , onloadCallback :: Eff eff Unit
  }


reCaptcha :: forall eff
           . ReCaptchaProps eff -> ReactElement
reCaptcha ps = createElement reCaptchaImpl ps []
