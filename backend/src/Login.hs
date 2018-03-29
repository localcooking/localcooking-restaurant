{-# LANGUAGE
    OverloadedStrings
  #-}

module Login where

import Data.Aeson (ToJSON (..), (.=), object, Value (String))
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS


data AuthError
  = FBLoginReturnBad BS.ByteString BS.ByteString
  | FBLoginReturnDenied BS.ByteString
  | FBLoginReturnBadParse

instance ToJSON AuthError where
  toJSON x = case x of
    FBLoginReturnBad code msg -> object
      [ "fbBad" .= object
        [ "code" .= T.decodeUtf8 code
        , "msg" .= T.decodeUtf8 msg
        ]
      ]
    FBLoginReturnDenied desc -> object
      [ "fbDenied" .= object
        [ "desc" .= T.decodeUtf8 desc
        ]
      ]
    FBLoginReturnBadParse -> String "bad-parse"
