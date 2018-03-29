module Types.Env where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either)
import Login.Error (AuthError)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AuthToken (AuthToken)


type EnvImpl =
  { development :: Boolean
  , facebookClientID :: String
  , authToken :: Maybe (Either AuthError AuthToken)
  , salt :: HashedPassword
  }

foreign import envImpl :: EnvImpl


type Env =
  { development :: Boolean
  , facebookClientID :: String
  , authToken :: Maybe (Either AuthError AuthToken)
  , salt :: HashedPassword
  }


env :: Env
env =
  { development: envImpl.development
  , facebookClientID: envImpl.facebookClientID
  , authToken: envImpl.authToken
  , salt: envImpl.salt
  }


