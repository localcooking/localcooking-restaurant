{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module Types.FrontendEnv where

import LocalCooking.Common.Password (HashedPassword)
import Login (ThirdPartyLoginToken)
import Facebook.App (FacebookClientId)

import qualified Data.Text as T
import Data.Aeson (ToJSON (..), (.=), object)



data FrontendEnv = FrontendEnv
  { frontendEnvDevelopment :: Bool
  , frontendEnvFacebookClientID :: FacebookClientId
  , frontendEnvLoginToken :: Maybe ThirdPartyLoginToken
  , frontendEnvSalt :: HashedPassword
  }

instance ToJSON FrontendEnv where
  toJSON
    FrontendEnv
      { frontendEnvDevelopment
      , frontendEnvFacebookClientID
      , frontendEnvLoginToken
      , frontendEnvSalt
      } = object
    [ "development" .= frontendEnvDevelopment
    , "facebookClientID" .= frontendEnvFacebookClientID
    , "loginToken" .= frontendEnvLoginToken
    , "salt" .= frontendEnvSalt
    ]
