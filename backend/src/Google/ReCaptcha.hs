{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , OverloadedLists
  #-}

module Google.ReCaptcha where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Data.URI (URI (..))
import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (Host))
import qualified Data.Strict.Maybe as Strict


newtype ReCaptchaSiteKey = ReCaptchaSiteKey
  { getReCaptchaSiteKey :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)

newtype ReCaptchaSecret = ReCaptchaSecret
  { getReCaptchaSecret :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)




googleReCaptchaAssetURI :: URI
googleReCaptchaAssetURI =
  URI
    (Strict.Just "https")
    True
    (URIAuth Strict.Nothing (Host ["www","google"] "com") Strict.Nothing)
    ["recaptcha", "api.js"]
    []
    Strict.Nothing
