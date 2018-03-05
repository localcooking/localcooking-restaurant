module Env where

import Prelude


type EnvImpl =
  { development :: Boolean
  , facebookClientID :: String
  }

foreign import envImpl :: EnvImpl


type Env =
  { development :: Boolean
  , facebookClientID :: String
  }


env :: Env
env =
  { development: envImpl.development
  , facebookClientID: envImpl.facebookClientID
  }


