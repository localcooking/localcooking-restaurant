module Types.Env where

import LocalCooking.Types.Env (Env)

import Prelude
import Data.Either (Either (Right))
import Data.Argonaut (Json, decodeJson)
import Partial.Unsafe (unsafePartial)


type EnvImpl =
  { development :: Boolean
  , facebookClientID :: String
  , googleReCaptchaSiteKey :: String
  , authToken :: Json
  , emailToken :: Json
  , formData :: Json
  , salt :: Json
  }

foreign import envImpl :: EnvImpl


env :: Env
env =
  { development: envImpl.development
  , facebookClientID: envImpl.facebookClientID
  , googleReCaptchaSiteKey: envImpl.googleReCaptchaSiteKey
  , authToken: unsafePartial $ case decodeJson envImpl.authToken of
    Right x -> x
  , emailToken: unsafePartial $ case decodeJson envImpl.emailToken of
    Right x -> x
  , salt: unsafePartial $ case decodeJson envImpl.salt of
    Right x -> x
  , formData: unsafePartial $ case decodeJson envImpl.formData of
    Right x -> x
  }
