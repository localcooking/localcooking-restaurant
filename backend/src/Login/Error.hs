{-# LANGUAGE
    OverloadedStrings
  #-}

module Login.Error where

import LocalCooking.Common.AuthToken (AuthToken)

import Data.Aeson (ToJSON (..), FromJSON (..), (.:), (.=), object, Value (String, Object))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Control.Applicative ((<|>))


data AuthError
  = FBLoginReturnBad Text Text
  | FBLoginReturnDenied Text
  | FBLoginReturnBadParse
  | FBLoginReturnNoUser

instance ToJSON AuthError where
  toJSON x = case x of
    FBLoginReturnBad code msg -> object
      [ "fbBad" .= object
        [ "code" .= code
        , "msg" .= msg
        ]
      ]
    FBLoginReturnDenied desc -> object
      [ "fbDenied" .= object
        [ "desc" .= desc
        ]
      ]
    FBLoginReturnBadParse -> String "bad-parse"
    FBLoginReturnNoUser -> String "no-user"

instance FromJSON AuthError where
  parseJSON json = case json of
    Object o -> do
      let denied = do
            o' <- o .: "fbDenied"
            FBLoginReturnDenied <$> o' .: "desc"
          bad = do
            o' <- o .: "fbBad"
            FBLoginReturnBad <$> o' .: "code" <*> o' .: "msg"
      denied <|> bad
    String s
      | s == "bad-parse" -> pure FBLoginReturnBadParse
      | s == "no-user" -> pure FBLoginReturnNoUser
      | otherwise -> fail
    _ -> fail
    where
      fail = typeMismatch "AuthError" json



newtype PreliminaryAuthToken = PreliminaryAuthToken
  { getPreliminaryAuthToken :: Maybe (Either AuthError AuthToken)
  }

instance ToJSON PreliminaryAuthToken where
  toJSON (PreliminaryAuthToken mTkn) = case mTkn of
    Nothing -> toJSON (Nothing :: Maybe ())
    Just eTkn -> case eTkn of
      Left e -> object ["err" .= e]
      Right tkn -> object ["token" .= tkn]
