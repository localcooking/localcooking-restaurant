{-# LANGUAGE
    OverloadedStrings
  #-}

module Login where

import Login.Facebook (FacebookLoginUserAccessToken)
import Data.Aeson (ToJSON (..), (.=), object)



data ThirdPartyLoginToken
  = FacebookLoginToken FacebookLoginUserAccessToken


instance ToJSON ThirdPartyLoginToken where
  toJSON x = case x of
    FacebookLoginToken token -> object [ "facebookToken" .= token ]
