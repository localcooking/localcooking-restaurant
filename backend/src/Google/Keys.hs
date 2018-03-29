{-# LANGUAGE
    OverloadedStrings
  #-}

module Google.Keys where

import Google.Analytics (GoogleAnalyticsGTag)

import Data.Aeson (FromJSON (..), (.:), Value (Object))
import Data.Aeson.Types (typeMismatch)


data GoogleCredentials = GoogleCredentials
  { googleAnalytics :: GoogleAnalyticsGTag
  } deriving (Eq, Show)


instance FromJSON GoogleCredentials where
  parseJSON json = case json of
    Object o -> GoogleCredentials <$> o .: "analytics"
    _ -> fail
    where
      fail = typeMismatch "GoogleCredentials" json
