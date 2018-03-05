{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module Types.FrontendEnv where

import Login (ThirdPartyLoginToken)

import qualified Data.Text as T
import Data.Aeson (ToJSON (..), (.=), object)



data FrontendEnv = FrontendEnv
  { frontendEnvDevelopment :: Bool
  , frontendEnvFacebookClientID :: T.Text
  , frontendEnvLoginToken :: Maybe ThirdPartyLoginToken
  }

instance ToJSON FrontendEnv where
  toJSON
    FrontendEnv
      { frontendEnvDevelopment
      , frontendEnvFacebookClientID
      , frontendEnvLoginToken
      } = object
    [ "development" .= frontendEnvDevelopment
    , "facebookClientID" .= frontendEnvFacebookClientID
    , "loginToken" .= frontendEnvLoginToken
    ]
