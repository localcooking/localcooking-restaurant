{-# LANGUAGE
    OverloadedStrings
  #-}

module Login where

import Facebook.Types (FacebookUserAccessToken)
import Data.Aeson (ToJSON (..), (.=), object)



data ThirdPartyLoginToken
  = FacebookLoginToken FacebookUserAccessToken


instance ToJSON ThirdPartyLoginToken where
  toJSON x = case x of
    FacebookLoginToken token -> object [ "facebookToken" .= token ]
