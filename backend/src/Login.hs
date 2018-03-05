{-# LANGUAGE
    OverloadedStrings
  #-}

module Login where

import qualified Data.Text as T
import Data.Aeson (ToJSON (..), (.=), object)



data ThirdPartyLoginToken
  = FacebookLoginToken T.Text


instance ToJSON ThirdPartyLoginToken where
  toJSON x = case x of
    FacebookLoginToken token -> object [ "facebookToken" .= token ]
