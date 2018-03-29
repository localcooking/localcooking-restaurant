{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module Types.FrontendEnv where

import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Common.Password (HashedPassword)
import Login (AuthError)
import Facebook.App (FacebookClientId)

import qualified Data.Text as T
import Data.Aeson (ToJSON (..), (.=), object)



data FrontendEnv = FrontendEnv
  { frontendEnvDevelopment :: Bool
  , frontendEnvFacebookClientID :: FacebookClientId
  , frontendEnvAuthToken :: Maybe (Either AuthError AuthToken)
  , frontendEnvSalt :: HashedPassword
  }

instance ToJSON FrontendEnv where
  toJSON
    FrontendEnv
      { frontendEnvDevelopment
      , frontendEnvFacebookClientID
      , frontendEnvAuthToken
      , frontendEnvSalt
      } = object
    [ "development" .= frontendEnvDevelopment
    , "facebookClientID" .= frontendEnvFacebookClientID
    , "authToken" .= frontendEnvAuthToken
    , "salt" .= frontendEnvSalt
    ]
