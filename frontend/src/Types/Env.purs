module Types.Env where

import Prelude
import Data.Either (Either (Right))
import Data.Argonaut (Json, decodeJson)
import Login.Error (PreliminaryAuthToken)
import LocalCooking.Common.Password (HashedPassword)
import Partial.Unsafe (unsafePartial)


type EnvImpl =
  { development :: Boolean
  , facebookClientID :: String
  , googleReCaptchaSiteKey :: String
  , authToken :: Json
  , salt :: Json
  }

foreign import envImpl :: EnvImpl


type Env =
  { development :: Boolean
  , facebookClientID :: String
  , googleReCaptchaSiteKey :: String
  , authToken :: PreliminaryAuthToken
  , salt :: HashedPassword
  }


env :: Env
env =
  { development: envImpl.development
  , facebookClientID: envImpl.facebookClientID
  , googleReCaptchaSiteKey: envImpl.googleReCaptchaSiteKey
  , authToken: unsafePartial $ case decodeJson envImpl.authToken of
    Right x -> x
  , salt: unsafePartial $ case decodeJson envImpl.salt of
    Right x -> x
  }
