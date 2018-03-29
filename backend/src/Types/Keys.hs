{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}


module Types.Keys where

import Data.Aeson (FromJSON (..), Value (Object), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Facebook.App (Credentials)



data Keys = Keys
  { keysFacebook :: Credentials
  }



instance FromJSON Keys where
  parseJSON (Object o) = do
    keysFacebook <- o .: "facebook"
    pure Keys{keysFacebook}
  parseJSON x = typeMismatch "Keys" x
