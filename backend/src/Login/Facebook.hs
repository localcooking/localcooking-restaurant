{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module Login.Facebook where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), Value (..))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))


data FacebookLoginReturn a
  = FacebookLoginReturnBad
      { facebookLoginBadErrorCode :: BS.ByteString
      , facebookLoginBadErrorMessage :: BS.ByteString
      }
  | FacebookLoginReturnGood
      { facebookLoginGoodCode :: FacebookLoginCode
      , facebookLoginGoodState :: a
      }
  | FacebookLoginReturnDenied
      { facebookLoginDeniedErrorDescription :: BS.ByteString
      }
  deriving (Show)


data FacebookLoginGetToken
  = FacebookLoginGetToken
    { facebookLoginGetTokenAccessToken :: FacebookLoginUserAccessToken
    , facebookLoginGetTokenTokenType :: T.Text
    , facebookLoginGetTokenExpiresIn :: Int
    }
  | FacebookLoginGetTokenError
    { facebookLoginGetTokenErrorMessage :: T.Text
    , facebookLoginGetTokenErrorType :: T.Text
    , facebookLoginGetTokenErrorCode :: Int
    , facebookLoginGetTokenErrorFbTraceID :: T.Text
    }
  deriving (Show)

instance FromJSON FacebookLoginGetToken where
  parseJSON (Object o) = do
    let good = do
          facebookLoginGetTokenAccessToken <- o .: "access_token"
          facebookLoginGetTokenTokenType <- o .: "token_type"
          facebookLoginGetTokenExpiresIn <- o .: "expires_in"
          pure FacebookLoginGetToken
            { facebookLoginGetTokenAccessToken
            , facebookLoginGetTokenTokenType
            , facebookLoginGetTokenExpiresIn
            }
        error' = do
          o' <- o .: "error"
          facebookLoginGetTokenErrorMessage <- o' .: "message"
          facebookLoginGetTokenErrorType <- o' .: "type"
          facebookLoginGetTokenErrorCode <- o' .: "code"
          facebookLoginGetTokenErrorFbTraceID <- o' .: "fbtrace_id"
          pure FacebookLoginGetTokenError
            { facebookLoginGetTokenErrorMessage
            , facebookLoginGetTokenErrorType
            , facebookLoginGetTokenErrorCode
            , facebookLoginGetTokenErrorFbTraceID
            }
    good <|> error'
  parseJSON x = typeMismatch "FacebookLoginGetToken" x


newtype FacebookLoginCode = FacebookLoginCode
  { getFacebookLoginCode :: T.Text
  } deriving (Eq, Show)

instance FromJSON FacebookLoginCode where
  parseJSON (String x) = pure (FacebookLoginCode x)
  parseJSON x = typeMismatch "FacebookLoginCode" x

instance ToJSON FacebookLoginCode where
  toJSON (FacebookLoginCode x) = String x


newtype FacebookLoginUserAccessToken = FacebookLoginUserAccessToken
  { getFacebookLoginUserAccessToken :: T.Text
  } deriving (Eq, Show)

instance FromJSON FacebookLoginUserAccessToken where
  parseJSON (String x) = pure (FacebookLoginUserAccessToken x)
  parseJSON x = typeMismatch "FacebookLoginUserAccessToken" x

instance ToJSON FacebookLoginUserAccessToken where
  toJSON (FacebookLoginUserAccessToken x) = String x


-- data FacebookLoginOrigin
--   = FacebookLoginOriginApp SiteLinks
--   | FacebookLoginOriginRegister
--       { facebookLoginOriginRegisterName :: Text
--       , facebookLoginOriginRegisterUsername :: Text
--       , facebookLoginOriginRegisterAddress :: Text
--       }


-- data FacebookLoginState = FacebookLoginState
--   { facebookLoginStateOrigin :: SiteLinks
--   , facebookLoginStateAux :: forall a. a
--   }
