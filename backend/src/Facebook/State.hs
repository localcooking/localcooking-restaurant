{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  #-}

module Facebook.State where

import Links (SiteLinks)

import Data.Aeson (ToJSON (..), FromJSON (..), object, (.=), (.:), Value (Object))
import Data.Aeson.Types (typeMismatch)


data FacebookLoginState = FacebookLoginState
  { facebookLoginStateOrigin :: SiteLinks
  }

instance ToJSON FacebookLoginState where
  toJSON FacebookLoginState{..} = object
    [ "origin" .= facebookLoginStateOrigin
    ]

instance FromJSON FacebookLoginState where
  parseJSON json = case json of
    Object o -> FacebookLoginState <$> o .: "origin"
    _ -> fail
    where
      fail = typeMismatch "FacebookLoginState" json
