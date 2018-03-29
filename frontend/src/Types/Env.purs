module Types.Env where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either)
import Login.Error (PreliminaryAuthToken)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.AuthToken (AuthToken)


type EnvImpl =
  { development :: Boolean
  , facebookClientID :: String
  , authToken :: PreliminaryAuthToken
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
  , authToken: envImpl.authToken
  , salt: envImpl.salt
  }
