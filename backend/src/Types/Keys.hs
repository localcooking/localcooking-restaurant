{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}


module Types.Keys where

import Data.Aeson (FromJSON (..), Value (Object), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T




data Keys = Keys
  { keysFacebookClientSecret :: T.Text
  , keysFacebookClientID :: T.Text
  }



instance FromJSON Keys where
  parseJSON (Object o) = do
    keysFacebookClientSecret <- o .: "facebook-client-secret"
    keysFacebookClientID <- o .: "facebook-client-id"
    pure Keys{keysFacebookClientSecret,keysFacebookClientID}
  parseJSON x = typeMismatch "Keys" x
