module Types.Env where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either (Right))
import Data.Argonaut (Json, decodeJson)
import Login.Error (PreliminaryAuthToken)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AuthToken (AuthToken)
import Partial.Unsafe (unsafePartial)


type EnvImpl =
  { development :: Boolean
  , facebookClientID :: String
  , authToken :: Json
  , salt :: HashedPassword
  }

foreign import envImpl :: EnvImpl


type Env =
  { development :: Boolean
  , facebookClientID :: String
  , authToken :: PreliminaryAuthToken
  , salt :: HashedPassword
  }


env :: Env
env =
  { development: envImpl.development
  , facebookClientID: envImpl.facebookClientID
  , authToken: unsafePartial $ case decodeJson envImpl.authToken of
    Right x -> x
  , salt: envImpl.salt
  }
